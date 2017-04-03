/**
 * file name : Test10ForBitMap.java
 * created at : 下午11:58:24 2016年8月11日
 * created by 970655147
 */

package com.hx.log.test;

import static com.hx.log.util.Log.log;
import static com.hx.log.util.Log.logHorizon;

import java.util.BitSet;

import com.hx.log.util.BitMap;
import com.hx.log.util.interf.IdxIterator;
import com.hx.log.util.interf.IdxIterator.SomeBitIncIdxIterator;

public class Test10ForBitMap {
	
	// 测试BitMap
	public static void main(String[] args) {
		
		log.logPatternChain = null;
		
//		BitMap bitMap = new BitMap(4, 2);
		BitMap bitMap = new BitMap(4, 2, false);
		
		log(bitMap.get(0) );
		bitMap.set(0, 1);
		log(bitMap.get(0) );
		bitMap.set(0, 5);
		log(bitMap.get(0) );
//		bitMap.set(0, -5);
//		log(bitMap.get(0) );
//		bitMap.set(0, -8);
//		log(bitMap.get(0) );
		bitMap.set(0, 12);
		log(bitMap.get(0) );
		bitMap.set(0, 8);
		log(bitMap.get(0) );
		
		bitMap.set(0, 8);
		log(bitMap.get(0) );
		
//		bitMap.set(3, 2);
		
		logHorizon();
		bitMap = BitMap.newUnsignedInt(123);
		for(int i=0, len=bitMap.getCapacity(); i<len; i++) {
			log(bitMap.get(i) );
		}
		logHorizon();
		BitMap copyBitMap = BitMap.copyOf(bitMap);
		for(int i=0, len=copyBitMap.getCapacity(); i<len; i++) {
			log(copyBitMap.get(i) );
		}
		
		BitSet bs = new BitSet();
		bs.set(12);
		bs.set(14);
		log(bs.size() );
		log(bs.length() );
		log(bs.cardinality() );
		logHorizon();
		int start = -1;
		while((start = bs.nextSetBit(start+1)) > 0) {
			log(start);
		}
		
		bs = new BitSet();
			bs.set(0);
			bs.set(2);
			bs.set(3);
		// ?2? = new SomeBitInc(24, 1020, bs)
		IdxIterator ite = new SomeBitIncIdxIterator(24, 1020, bs);
		while(ite.hasNext() ) {
			log(ite.next() );
		}
		
		logHorizon();
		BitMap bm = BitMap.newUnsignedInt(929, 4);
		log(SomeBitIncIdxIterator.incBit(bm, bs) );
		log(SomeBitIncIdxIterator.peek(bm) );
		log(SomeBitIncIdxIterator.incBit(bm, bs) );
		
//		log(2 >> 0);
//		log(2 >> -1);
//		log(2 << 1);
		
//		log(BitMap.bitSizeFor(3) );
//		log(BitMap.bitSizeFor(5) );
		
	}

}
