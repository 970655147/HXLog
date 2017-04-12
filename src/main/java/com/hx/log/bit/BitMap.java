/**
 * file name : BitMap.java
 * created at : ����10:56:39 2016��8��11��
 * created by 970655147
 */

package com.hx.log.bit;

import com.hx.log.util.Tools;

import java.util.BitSet;

//��bitPerData��bit��Ϊһ������
//Լ�����λΪ����λ, ʣ���bitΪ����λ
	// Լ�� 00...0 Ϊ (-maxPos-1)
// 0001 -> +1
// 1001 -> -1
// 0000 -> 0
// 1000 -> -8
// �޷��ŵĹ���, �����е�bit��Ϊ����λ
// 0001 -> 1
// 1001 -> 129
// 0000 -> 0
// 1000 -> 128
public final class BitMap {
	
	// ������ݵ�����, һ������ռ�ö��ٸ�bit, ��ǰBitMap����������
	private int bitPerData;
	private int capacity;
	private boolean signed;
	
	private BitSet data;
	private int maxPos;
	private int minNeg;
	private int leftShift;
	
	// ��ʼ��
	// this.bitPerData = bitSizeFor(bitPerData); ��Ϊ�˼����������Ż�
	// ������ȡ�����ӿ�, �Ϳ����Զ����Ƿ��Ż��Ĳ�����[bitSizeFor, calcByteTh, calcBitOff]
	public BitMap(int bitPerData, int capacity, boolean signed) {
		Tools.assert0(((bitPerData > 0) && (bitPerData < 32) ), "'bitPerData' must in [0, 32) !");
		Tools.assert0((capacity > 0), "'capacity' must in [0, #) !");
		
		this.bitPerData = bitPerData;
		this.capacity = capacity;
		this.signed = signed;
		maxPos = calcMaxPos(this.bitPerData-1);
		minNeg = (- maxPos) - 1;
		// -128 <-> 127
		// 		|| + 128
		//      \/
		// 	  0 <-> 255
		if(! signed) {
			minNeg += (maxPos + 1);
			maxPos += (maxPos + 1);
		}
		data = new BitSet(calcByteCnt(this.bitPerData * this.capacity) );
		leftShift = -1;
		leftShift = ((bitPerData & bitPerData-1) == 0) ? calcShfitBit(bitPerData) : leftShift;
	}
	public BitMap(int bitPerData, int capacity) {
		this(bitPerData, capacity, true);
	}
	
	// ���߷���
	// �ӵ�λ����λ��������
	public static BitMap newUnsignedInt(int val) {
		val = (val < 0) ? -val : val;
		return newUnsignedInt(val, String.valueOf(val).length() );
	}
	public static BitMap newUnsignedInt(int val, int cap) {
		// 4bit : 0 - 9
		BitMap result = new BitMap(4, cap, false);
		return setInt(result, val);
	}
	public static BitMap setInt(BitMap result, int val) {
		val = (val < 0) ? -val : val;
		String valStr = String.valueOf(val);
		if(Tools.isEmpty(valStr) ) {
			return result;
		}
		
		int idx = 0;
		for(int i=valStr.length()-1; i>=0; i--) {
			result.set(idx++, valStr.charAt(i)-'0');
		}
		
		return result;
	}
	public static BitMap copyOf(BitMap bitMap) {
		BitMap result = new BitMap(bitMap.getBitPerData(), bitMap.getCapacity(), bitMap.isSigned() );
		result.data.or(bitMap.data);
		return result;
	}
	
	// set(i)
	public BitMap set(int idx, int _data) {
		Tools.assert0(((idx >= 0) && (idx < capacity) ), "'idx'[" + idx + "] not in range [0, " + capacity + ") !");
		Tools.assert0(((_data >= minNeg) && (_data <= maxPos) ), "'data'[" + _data + "] not in range [" + minNeg + ", " + maxPos + "] !");
		
		int lower = calcLowerBit(idx);
		int higher = (lower + bitPerData) - 1;
		// �����Ƿ���00..00, ����"�Ǹ���" ���µ�һ��bit
		if(_data == minNeg) {
			if(signed) {
				data.set(higher);
			} else {
				data.clear(higher);
			}
			data.clear(lower, higher);
			return this;
		}
		
		boolean isNeg = (_data < 0) ? true : false;
		_data = isNeg ? - _data : _data;
		// update signed
		if(! signed) {
			isNeg = _data >= (((maxPos-minNeg) + 1) >> 1);
		}
		if(isNeg) {
			data.set(higher);
		} else {
			data.clear(higher);
		}
		for(int i=0, dataLen=bitPerData-1; i<dataLen; i++) {
			// low -> high		// data & 1; _data >>= 1; | bitOff-(bitPerData-1)+i
			if((_data & 1) != 0) {
				data.set(lower+i);
			} else {
				data.clear(lower+i);
			}
			_data >>= 1;
		}
		
		return this;
	}
	
	// get(i)
	// signed			unsigned		update
	// -128 - 127		0 - 255
	// 1 00..00[-128]	128				spec
	// 1 00..01[-1]		129				128 - i
	// 1 11..11[-127]	255				128 - i
	// 0 00..00[0]		0				i
	// 0 00..01[1]		1				i
	public int get(int idx) {
		Tools.assert0(((idx >= 0) && (idx < capacity) ), "'idx'[" + idx + "] not in range !");
		
		int lower = calcLowerBit(idx);
		int higher = (lower + bitPerData) - 1;
		boolean isNeg = data.get(higher); 
		int result = 0;
		for(int i=0, dataLen=bitPerData-1; i<dataLen; i++) {
			// high -> low		// bitOff - i - 1
			result <<= 1;
			if(data.get(higher-i-1) ) {
				result ++;
			}
		}
		
		// ����singed, unsigned��������
		if(isNeg) {
			if(result == 0) {
				if(signed) {
					result = minNeg;
				} else {
					result = ((maxPos-minNeg) + 1) >> 1;
				}
			} else {
				if(signed) {
					result = - result;
				} else {
					result = (((maxPos-minNeg) + 1) >> 1) + result;
				}
			}
		}
		
		return result;
	}
	public int getBitPerData() {
		return bitPerData;
	}
	public int getCapacity() {
		return capacity;
	}
	public boolean isSigned() {
		return signed;
	}
	
	// ---------------------------- �������� ----------------------------
	// ������Ҫ��bit������, ������Ҫ���ֽڵĸ���
	private int calcByteCnt(int allBit) {
		return ((allBit - 1) >> 3) + 1;
	}
	private int calcMaxPos(int bitPerData) {
		return (2 << (bitPerData-1) ) - 1;
	}
	private int calcLowerBit(int idx) {
		if(leftShift >= 0) {
			return idx << leftShift;
		} else {
			return idx * bitPerData;
		}
	}
	public int calcShfitBit(int bitPerData) {
		switch(bitPerData) {
			case 1 : return 0;
			case 2 : return 1;
			case 4 : return 2;
			case 8 : return 3;
			case 16 : return 4;
			case 32 : return 5;
			default : throw new RuntimeException("unsupported 'bitPerData': " + bitPerData);
		}
	}
	
}

// ---------------- һ���򵥵�ʵ��, ���� ���кܶ಻���Ƶĵط�, ���ܴ���bitPerData�ĳ���[��BitSet��ʵ��֮ǰ��ʵ��] ------------------------
// ��bitPerData��bit��Ϊһ������
// Լ�����λΪ����λ, ʣ���bitΪ����λ
	// Լ�� 00...0 Ϊ (-maxPos-1)
// 0001 -> +1
// 1001 -> -1
// 0000 -> -8
//public class BitMap {
//
//	// ������ݵ�����, һ������ռ�ö��ٸ�bit, ��ǰBitMap����������
//	private byte[] data;
//	private int bitPerData;
//	private int maxPos;
//	private int minNeg;
//	private int capacity;
//	
//	// ��ʼ��
//	// this.bitPerData = bitSizeFor(bitPerData); ��Ϊ�˼����������Ż�
//	// ������ȡ�����ӿ�, �Ϳ����Զ����Ƿ��Ż��Ĳ�����[bitSizeFor, calcByteTh, calcBitOff]
//	public BitMap(int bitPerData, int capacity) {
//		Tools.assert0(((bitPerData > 0) && (bitPerData < 32) ), "'bitPerData' must in [0, 32) !");
//		Tools.assert0((capacity > 0), "'capacity' must in [0, #) !");
//		
//		this.bitPerData = bitSizeFor(bitPerData);
//		this.capacity = capacity;
//		maxPos = calcMaxPos(this.bitPerData-1);
//		minNeg = (- maxPos) - 1;
//		data = new byte[calcByteCnt(this.bitPerData * this.capacity) ];
//	}
//	
//	// set(i)
//	public void set(int idx, int _data) {
//		Tools.assert0(((idx >= 0) && (idx < capacity) ), "'idx'[" + idx + "] not in range [0, " + capacity + ") !");
//		Tools.assert0(((_data >= minNeg) && (_data <= maxPos) ), "'data'[" + _data + "] not in range [" + minNeg + ", " + maxPos + "] !");
//		
//		int byteTh = calcByteTh(idx);
//		//   high <- low
//		// 7 6 5 4 - 3 2 1 0	// bit order
//		// 1 0 0 1 - 0 1 0 1	// bit data
//		int bitOff = calcBitOff(idx);
//		if(_data == minNeg) {
//			data[byteTh] |= (1 << bitOff);
//			for(int i=0, dataLen=bitPerData-1; i<dataLen; i++) {
//				data[byteTh] &= (~ (1 << (bitOff-(bitPerData-1)+i)) );
//			}
//			return ;
//		}
//		
//		boolean isNeg = (_data < 0) ? true : false;
//		_data = isNeg ? - _data : _data;
//		if(isNeg) {
//			data[byteTh] |= (1 << bitOff);
//		} else {
//			data[byteTh] &= (~ (1 << bitOff));
//		}
//		for(int i=0, dataLen=bitPerData-1; i<dataLen; i++) {
//			// low -> high		// data & 1; _data >>= 1; | bitOff-(bitPerData-1)+i
//			if((_data & 1) != 0) {
//				data[byteTh] |= (1 << (bitOff-(bitPerData-1)+i) );
//			} else {
//				data[byteTh] &= (~ (1 << (bitOff-(bitPerData-1)+i)) );
//			}
//			_data >>= 1;
//		}
//	}
//	
//	// get(i)
//	public int get(int idx) {
//		Tools.assert0(((idx >= 0) && (idx < capacity) ), "'idx'[" + idx + "] not in range !");
//		
//		int byteTh = calcByteTh(idx);
//		//   high <- low
//		// 7 6 5 4 - 3 2 1 0	// bit order
//		// 1 0 0 1 - 0 1 0 1	// bit data
//		int bitOff = calcBitOff(idx);
//		boolean isNeg = ((data[byteTh] & (1<<bitOff) ) != 0) ? true : false; 
//		int result = 0;
//		for(int i=0, dataLen=bitPerData-1; i<dataLen; i++) {
//			// high -> low		// bitOff - i - 1
//			result <<= 1;
//			if((data[byteTh] & (1 << (bitOff-i-1)) ) != 0) {
//				result ++;
//			}
//		}
//		if(isNeg) {
//			if(result == 0) {
//				result = minNeg;
//			} else {
//				result = - result;
//			}
//		}
//		
//		return result;
//	}
//	
//	// ---------------------------- �������� ----------------------------
//	// ������Ҫ��bit������, ������Ҫ���ֽڵĸ���
//	private int calcByteCnt(int allBit) {
//		return ((allBit - 1) >> 3) + 1;
//	}
//	private int calcMaxPos(int bitPerData) {
//		return (2 << (bitPerData-1) ) - 1;
//	}
//	private int calcByteTh(int i) {
//		switch(bitPerData) {
//			case 1: 
//				return i >> 3;
//			case 2:
//				return i >> 2;
//			case 4:
//				return i >> 1;
//			case 8:
//				return i >> 0;
//			case 16:
//				return i << 1;
//			case 32:
//				return i << 2;
//			default :
//				throw new RuntimeException("have not this 'bitPerData' !");
//		}
//	}
//	private int calcBitOff(int i) {
//		if(bitPerData >= 8) {
//			return 0;
//		} else {
//			return ((i & (calcByteTh(8)-1)) << calcShiftLeftBitOff()) + bitPerData - 1;
//		}
//	}
//	private int calcShiftLeftBitOff() {
//		switch(bitPerData) {
//			case 1: 
//				return 0;
//			case 2:
//				return 1;
//			case 4:
//				return 2;
//			case 8:
//				return 3;
//			default :
//				throw new RuntimeException("have not this 'bitPerData' !");
//		}
//	}
//    private static final int bitSizeFor(int cap) {
//        int n = cap - 1;
//        n |= n >>> 1;
//        n |= n >>> 2;
//        n |= n >>> 4;
//        n |= n >>> 8;
//        n |= n >>> 16;
//        return n + 1;
//    }
//	
//}
