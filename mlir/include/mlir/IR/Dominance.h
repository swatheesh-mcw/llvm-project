//===- Dominance.h - Dominator analysis for regions -------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// The DominanceInfo and PostDominanceInfo class provide routines for performimg
// simple dominance checks, and expose dominator trees for advanced clients.
// These classes provide fully region-aware functionality, lazily constructing
// dominator information for any multi-block regions that need it.
//
// For more information about the theory behind dominance in graphs algorithms,
// see: https://en.wikipedia.org/wiki/Dominator_(graph_theory)
//
//===----------------------------------------------------------------------===//

#ifndef MLIR_IR_DOMINANCE_H
#define MLIR_IR_DOMINANCE_H

#include "mlir/IR/RegionGraphTraits.h"
#include "llvm/Support/GenericDomTree.h"

extern template class llvm::DominatorTreeBase<mlir::Block, false>;
extern template class llvm::DominatorTreeBase<mlir::Block, true>;
extern template class llvm::DomTreeNodeBase<mlir::Block>;

namespace mlir {
using DominanceInfoNode = llvm::DomTreeNodeBase<Block>;
class Operation;

namespace detail {
template <bool IsPostDom>
class DominanceInfoBase {
  using DomTree = llvm::DominatorTreeBase<Block, IsPostDom>;

public:
  DominanceInfoBase(Operation *op = nullptr) {}
  DominanceInfoBase(DominanceInfoBase &&) = default;
  DominanceInfoBase &operator=(DominanceInfoBase &&) = default;
  ~DominanceInfoBase();

  DominanceInfoBase(const DominanceInfoBase &) = delete;
  DominanceInfoBase &operator=(const DominanceInfoBase &) = delete;

  /// Invalidate dominance info. This can be used by clients that make major
  /// changes to the CFG and don't have a good way to update it.
  void invalidate();
  void invalidate(Region *region);

  /// Finds the nearest common dominator block for the two given blocks a
  /// and b. If no common dominator can be found, this function will return
  /// nullptr.
  Block *findNearestCommonDominator(Block *a, Block *b) const;

  /// Finds the nearest common dominator block for the given range of blocks.
  /// If no common dominator can be found, this function will return nullptr.
  template <typename BlockRangeT>
  Block *findNearestCommonDominator(BlockRangeT &&blocks) const {
    if (blocks.begin() == blocks.end())
      return nullptr;
    Block *dom = *blocks.begin();
    for (auto it = ++blocks.begin(); it != blocks.end(); ++it) {
      dom = findNearestCommonDominator(dom, *it);
      if (!dom)
        return nullptr;
    }
    return dom;
  }

  /// Get the root dominance node of the given region. Note that this operation
  /// is only defined for multi-block regions!
  DominanceInfoNode *getRootNode(Region *region) {
    auto domInfo = getDominanceInfo(region, /*needsDomTree*/ true).getPointer();
    assert(domInfo && "Region isn't multiblock");
    return domInfo->getRootNode();
  }

  /// Return the dominance node from the Region containing block A. This only
  /// works for multi-block regions.
  DominanceInfoNode *getNode(Block *a) {
    return getDomTree(a->getParent()).getNode(a);
  }

  /// Return true if the specified block is reachable from the entry
  /// block of its region.
  bool isReachableFromEntry(Block *a) const;

  /// Return true if operations in the specified block are known to obey SSA
  /// dominance requirements. False if the block is a graph region or unknown.
  bool hasSSADominance(Block *block) const {
    return hasSSADominance(block->getParent());
  }
  /// Return true if operations in the specified block are known to obey SSA
  /// dominance requirements. False if the block is a graph region or unknown.
  bool hasSSADominance(Region *region) const {
    return getDominanceInfo(region, /*needsDomTree=*/false).getInt();
  }

  DomTree &getDomTree(Region *region) const {
    assert(!region->hasOneBlock() &&
           "Can't get DomTree for single block regions");
    return *getDominanceInfo(region, /*needsDomTree=*/true).getPointer();
  }

protected:
  using super = DominanceInfoBase<IsPostDom>;

  /// Return the dom tree and "hasSSADominance" bit for the given region. The
  /// DomTree will be null for single-block regions. This lazily constructs the
  /// DomTree on demand when needsDomTree=true.
  llvm::PointerIntPair<DomTree *, 1, bool>
  getDominanceInfo(Region *region, bool needsDomTree) const;

  /// Return "true" if block iterator A properly (post)dominates block iterator
  /// B. If `enclosingOk` is set, A is considered to (post)dominate B if A
  /// encloses B.
  bool properlyDominatesImpl(Block *aBlock, Block::iterator aIt, Block *bBlock,
                             Block::iterator bIt,
                             bool enclosingOk = true) const;

  /// A mapping of regions to their base dominator tree and a cached
  /// "hasSSADominance" bit. This map does not contain dominator trees for
  /// single block CFG regions, but we do want to cache the "hasSSADominance"
  /// bit for them. We may also not have computed the DomTree yet. In either
  /// case, the DomTree is just null.
  ///
  mutable DenseMap<Region *, llvm::PointerIntPair<DomTree *, 1, bool>>
      dominanceInfos;
};

extern template class DominanceInfoBase</*IsPostDom=*/true>;
extern template class DominanceInfoBase</*IsPostDom=*/false>;
} // namespace detail

/// A class for computing basic dominance information. Note that this
/// class is aware of different types of regions and returns a
/// region-kind specific concept of dominance. See RegionKindInterface.
class DominanceInfo : public detail::DominanceInfoBase</*IsPostDom=*/false> {
public:
  using super::super;

  /// Return true if operation A properly dominates operation B, i.e. if A and B
  /// are in the same block and A properly dominates B within the block, or if
  /// the block that contains A properly dominates the block that contains B. In
  /// an SSACFG region, Operation A dominates Operation B in the same block if A
  /// preceeds B. In a Graph region, all operations in a block properly dominate
  /// all operations in the same block.
  ///
  /// The `enclosingOpOk` flag says whether we should return true if the B op
  /// is enclosed by a region on A.
  bool properlyDominates(Operation *a, Operation *b,
                         bool enclosingOpOk = true) const;

  /// Return true if operation A dominates operation B, i.e. if A and B are the
  /// same operation or A properly dominates B.
  bool dominates(Operation *a, Operation *b) const {
    return a == b || properlyDominates(a, b);
  }

  /// Return true if the `a` value properly dominates operation `b`, i.e if the
  /// operation that defines `a` properlyDominates `b` and the operation that
  /// defines `a` does not contain `b`.
  bool properlyDominates(Value a, Operation *b) const;

  /// Return true if the `a` value dominates operation `b`.
  bool dominates(Value a, Operation *b) const {
    return (Operation *)a.getDefiningOp() == b || properlyDominates(a, b);
  }

  /// Return true if the specified block A dominates block B, i.e. if block A
  /// and block B are the same block or block A properly dominates block B.
  bool dominates(Block *a, Block *b) const {
    return a == b || properlyDominates(a, b);
  }

  /// Return true if the specified block A properly dominates block B, i.e.: if
  /// block A contains block B, or if the region which contains block A also
  /// contains block B or some parent of block B and block A dominates that
  /// block in that kind of region.
  ///
  /// In an SSACFG region, block A dominates block B if all control flow paths
  /// from the entry block to block B flow through block A.
  ///
  /// Graph regions have only a single block. To be consistent with "proper
  /// dominance" of ops, the single block is considered to properly dominate
  /// itself in a graph region.
  bool properlyDominates(Block *a, Block *b) const;

  bool properlyDominates(Block *aBlock, Block::iterator aIt, Block *bBlock,
                         Block::iterator bIt, bool enclosingOk = true) const {
    return super::properlyDominatesImpl(aBlock, aIt, bBlock, bIt, enclosingOk);
  }

  bool dominates(Block *aBlock, Block::iterator aIt, Block *bBlock,
                 Block::iterator bIt, bool enclosingOk = true) const {
    return (aBlock == bBlock && aIt == bIt) ||
           super::properlyDominatesImpl(aBlock, aIt, bBlock, bIt, enclosingOk);
  }
};

/// A class for computing basic postdominance information.
class PostDominanceInfo : public detail::DominanceInfoBase</*IsPostDom=*/true> {
public:
  using super::super;

  /// Return true if operation A properly postdominates operation B.
  bool properlyPostDominates(Operation *a, Operation *b,
                             bool enclosingOpOk = true) const;

  /// Return true if operation A postdominates operation B.
  bool postDominates(Operation *a, Operation *b) const {
    return a == b || properlyPostDominates(a, b);
  }

  /// Return true if the specified block A properly postdominates block B.
  bool properlyPostDominates(Block *a, Block *b) const;

  /// Return true if the specified block A postdominates block B.
  bool postDominates(Block *a, Block *b) const {
    return a == b || properlyPostDominates(a, b);
  }

  bool properlyPostDominates(Block *aBlock, Block::iterator aIt, Block *bBlock,
                             Block::iterator bIt,
                             bool enclosingOk = true) const {
    return super::properlyDominatesImpl(aBlock, aIt, bBlock, bIt, enclosingOk);
  }

  bool postDominates(Block *aBlock, Block::iterator aIt, Block *bBlock,
                     Block::iterator bIt, bool enclosingOk = true) const {
    return (aBlock == bBlock && aIt == bIt) ||
           super::properlyDominatesImpl(aBlock, aIt, bBlock, bIt, enclosingOk);
  }
};

} // namespace mlir

namespace llvm {

/// DominatorTree GraphTraits specialization so the DominatorTree can be
/// iterated by generic graph iterators.
template <>
struct GraphTraits<mlir::DominanceInfoNode *> {
  using ChildIteratorType = mlir::DominanceInfoNode::const_iterator;
  using NodeRef = mlir::DominanceInfoNode *;

  static NodeRef getEntryNode(NodeRef N) { return N; }
  static inline ChildIteratorType child_begin(NodeRef N) { return N->begin(); }
  static inline ChildIteratorType child_end(NodeRef N) { return N->end(); }
};

template <>
struct GraphTraits<const mlir::DominanceInfoNode *> {
  using ChildIteratorType = mlir::DominanceInfoNode::const_iterator;
  using NodeRef = const mlir::DominanceInfoNode *;

  static NodeRef getEntryNode(NodeRef N) { return N; }
  static inline ChildIteratorType child_begin(NodeRef N) { return N->begin(); }
  static inline ChildIteratorType child_end(NodeRef N) { return N->end(); }
};

} // namespace llvm
#endif
