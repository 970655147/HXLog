/**
 * file name : IdxIterator.java
 * created at : 下午9:47:52 2016年8月11日
 * created by 970655147
 */

package com.hx.log.interf;

import com.hx.log.bit.BitMap;
import com.hx.log.util.Tools;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

// 一个获取索引的迭代器
public interface IdxIterator {
	// iteratorMethods
	public boolean hasNext();
	public int next();
	
	// 没有数据的IdxIterator
	public static class NoneIdxIterator implements IdxIterator {
		private static NoneIdxIterator instance;
		private NoneIdxIterator() {
			
		}
		// 工具方法
		public static NoneIdxIterator getInstance() {
			if(instance == null) {
				synchronized (NoneIdxIterator.class) {
					if(instance == null) {
						instance = new NoneIdxIterator();
					}
				}
			}
			
			return instance;
		}
		@Override
		public boolean hasNext() {
			return false;
		}
		@Override
		public int next() {
			if(! hasNext() ) {
				throw new RuntimeException("have no next !");
			}
			return 0;
		}
	}
	
	// 对应于一个数字的IdxIterator
	public static class SingleIdxIterator implements IdxIterator {
		private int single;
		private boolean iterated;
		public SingleIdxIterator(int single) {
			this.single = single;
		}
		@Override
		public boolean hasNext() {
			return ! iterated;
		}
		@Override
		public int next() {
			if(! hasNext() ) {
				throw new RuntimeException("have no next !");
			}
			
			iterated = true;
			return single;
		}
	}
	
	// 对应于一个Range的IdxIterator '[start ... end)'
	public static class RangeIdxIterator implements IdxIterator {
//		private int start;
		private int end;
		private int cur;
		public RangeIdxIterator(int start, int end) {
//			this.start = start;
			this.end = end;
			this.cur = start;
		}
		@Override
		public boolean hasNext() {
			return cur < end;
		}
		@Override
		public int next() {
			if(! hasNext() ) {
				throw new RuntimeException("have no next !");
			}
			
			return cur ++;
		}
	}
	
	// 对应于一个累增的的IdxIterator '[[start ... end) += step]'
	public static class IncIdxIterator implements IdxIterator {
//		private int start;
		private int step;
		private int cnt;
		private int cur;
		private int executed;
		public IncIdxIterator(int start, int step, int cnt) {
//			this.start = start;
			this.step = step;
			this.cnt = cnt;
			this.cur = start;
			executed = 0;
		}
		@Override
		public boolean hasNext() {
			return executed < cnt;
		}
		@Override
		public int next() {
			if(! hasNext() ) {
				throw new RuntimeException("have no next !");
			}
			
			int result = cur;
			cur += step;
			executed ++;
			return result;
		}
		// 创建对应的Inc
		public static IncIdxIterator newEndInc(int start, int step, int end) {
			int cnt = ((end-1) - start) / step + 1;
			return new IncIdxIterator(start, step, cnt);
		}
		public static IncIdxIterator newCntInc(int start, int step, int cnt) {
			return new IncIdxIterator(start, step, cnt);
		}
	}
	
	// 对应于某些位需要累增的IdxIterator
	// ?3?
	// 030 - 039
	// ...
	// 930 - 939
	public static class SomeBitIncIdxIterator implements IdxIterator {
//		private BitMap start;
		private BitMap end;
		private BitMap cur;
		private int[] incBit;
		private int diffBit;
		public SomeBitIncIdxIterator(int start, int end, BitSet incBit) {
			Tools.assert0(end >= start, "'end' must get 'start' ");
			Tools.assert0(incBit != null, "'incBit' can't be null ");
			
			this.end = BitMap.newUnsignedInt(end);
//			this.start = BitMap.newInt(start, this.end.getCapacity() );
//			this.cur = BitMap.copyOf(this.start);
			this.cur = BitMap.newUnsignedInt(start, this.end.getCapacity() );
			this.incBit = bitSet2IdxArr(incBit);
			diffBit = this.end.getCapacity() - 1;
		}
		// 工具方法
		public static int incBit(BitMap cur, BitSet incBit) {
			return incBit(cur, bitSet2IdxArr(incBit) );
		}
		public static int incBit(BitMap cur, int[] incBit) {
			int result = peek(cur);
			
			int lowerIncBit = cur.get(incBit[0]);
			if(lowerIncBit < 9) {
				cur.set(incBit[0], lowerIncBit+1);
			} else {
				cur.set(incBit[0], 0);
				for(int i=1; i<incBit.length; i++) {
					int curIncBit = cur.get(incBit[i]);
					if(curIncBit < 9) {
						cur.set(incBit[i], curIncBit+1);
						break ;
					} else {
						cur.set(incBit[i], 0);
					}
				}
			}
			
			return result;
		}
		public static int peek(BitMap cur) {
			int radix = 10;
			int result = 0;
			for(int i=cur.getCapacity()-1; i>=0; i--) {
				result = radix * result + cur.get(i);
			}
			return result;
		}
		private static int[] bitSet2IdxArr(BitSet incBit) {
			int[] result = new int[incBit.cardinality()];
			int _start = -1, idx = 0;
			while((_start = incBit.nextSetBit(_start+1)) >= 0) {
				result[idx ++] = _start;
			}
			return result;
		}
		@Override
		public boolean hasNext() {
//			for(int i=end.getCapacity(); i>=0; i--) {
//				int endBit = end.get(i), curBit = cur.get(i);
//				if(endBit > curBit ) {
//					return true;
//				} else if(endBit < curBit) {
//					return false;
//				}
//				// else 'endBit == curBit', compare next
//			}
			if(diffBit < 0) {
				return false;
			}
			
			int endBit = end.get(diffBit), curBit = cur.get(diffBit);
			if(endBit > curBit ) {
				return true;
			} else if(endBit < curBit) {
				return false;
			// incase of the 'diffBit' eq 
			} else {
				for(int i=diffBit-1; i>=0; i--) {
					endBit = end.get(i); curBit = cur.get(i);
					if(endBit > curBit ) {
						diffBit = i;
						return true;
					} else if(endBit < curBit) {
						diffBit = i;
						return false;
					}
					// incase of the 'i' eq
				}
				diffBit = -1;
			}
			
			// all eq
			return false;
		}
		@Override
		public int next() {
			if(! hasNext() ) {
				throw new RuntimeException("have no next !");
			}
			
			return incBit(cur, incBit); 
		}
	}
	
	// idxIterator with 'filter'
	public static class FilteredIdxIterator implements IdxIterator {
		private IdxIterator ite;
		private IdxIteratorFilter filter;
		private int next;
		public FilteredIdxIterator(IdxIterator ite, IdxIteratorFilter filter) {
			this.ite = ite;
			this.filter = filter;
			this.next = -1;
		}
		@Override
		public boolean hasNext() {
			if(next >= 0) {
				return true;
			}
			while(ite.hasNext() ) {
				int _next = ite.next();
				if(filter.filter(_next) ) {
					next = _next;
					break ;
				}
			}
			
			return (next >= 0);
		}
		@Override
		public int next() {
			if(! hasNext() ) {
				throw new RuntimeException("have no next !");
			}
			int result = next;
			next = -1;
			return result;
		}
	}
	
	// IdxIteratorFilter
	public static interface IdxIteratorFilter {
		public boolean filter(int idx);
	}
	public static class UpperBoundsIdxIteratorFilter implements IdxIteratorFilter {
		private int max;
		private boolean containsEq;
		public UpperBoundsIdxIteratorFilter(int max, boolean containsEq) {
			this.max = max;
			this.containsEq = containsEq;
		}
		@Override
		public boolean filter(int idx) {
			return containsEq ? (idx <= max) : (idx < max);
		}
	}
	public static class LowerBoundsIdxIteratorFilter implements IdxIteratorFilter {
		private int min;
		private boolean containsEq;
		public LowerBoundsIdxIteratorFilter(int min, boolean containsEq) {
			this.min = min;
			this.containsEq = containsEq;
		}
		@Override
		public boolean filter(int idx) {
			return containsEq ? (idx >= min) : (idx > min);
		}
		
	}
	
	// filter '> max'
	public static class UpperBoundsIdxIterator implements IdxIterator {
		private FilteredIdxIterator ite;
		public UpperBoundsIdxIterator(IdxIterator ite, int max, boolean containsEq) {
			Tools.assert0(ite != null, "ite can't be null !");
			this.ite = new FilteredIdxIterator(ite, new UpperBoundsIdxIteratorFilter(max, containsEq) );
		}
		public UpperBoundsIdxIterator(IdxIterator ite, int max) {
			this(ite, max, true);
		}
		@Override
		public boolean hasNext() {
			return ite.hasNext();
		}
		@Override
		public int next() {
			return ite.next();
		}
	}
	public static class LowerBoundsIdxIterator implements IdxIterator {
		private FilteredIdxIterator ite;
		public LowerBoundsIdxIterator(IdxIterator ite, int min, boolean containsEq) {
			Tools.assert0(ite != null, "ite can't be null !");
			this.ite = new FilteredIdxIterator(ite, new LowerBoundsIdxIteratorFilter(min, containsEq) );
		}
		public LowerBoundsIdxIterator(IdxIterator ite, int min) {
			this(ite, min, true);
		}
		@Override
		public boolean hasNext() {
			return ite.hasNext();
		}
		@Override
		public int next() {
			return ite.next();
		}
	}
	
	// chain
	public static class ChainOfIdxIterator implements IdxIterator {
		public List<IdxIterator> chain;
		public int curIdx;
		public ChainOfIdxIterator() {
			this(new ArrayList<IdxIterator>() );
		}
		public ChainOfIdxIterator(List<IdxIterator> chain) {
			Tools.assert0(chain != null, "chain can't be null !");
			this.chain = chain;
			this.curIdx = 0;
		}
		public ChainOfIdxIterator add(IdxIterator idxIterator) {
			this.chain.add(idxIterator);
			return this;
		}
		@Override
		public boolean hasNext() {
			if(chain == null) {
				return false;
			}
			if(curIdx >= chain.size() ) {
				return false;
			}
			if(chain.get(curIdx).hasNext() ) {
				return true;
			}
			while(((++ curIdx) < chain.size() ) && chain.get(curIdx).hasNext() ) {
				return true;
			}
			
			return false;
		}
		@Override
		public int next() {
			if(! hasNext() ) {
				throw new RuntimeException("have no next !");
			}
			return chain.get(curIdx).next();
		}
	}
	
}
