package com.hx.log.idx.idx_iterator;

import com.hx.common.bit.BitMap;
import com.hx.common.interf.idx.IdxIterator;
import com.hx.log.util.Tools;

import java.util.BitSet;

/**
 * 对应于某些位需要累增的IdxIterator
 * ?3?
 * 030 - 039
 * ...
 * 930 - 939
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:02 PM
 */
public class SomeBitIncIdxIterator implements IdxIterator {

    /**
     * 上限的BitMap
     */
    private BitMap end;
    /**
     * 当前的索引
     */
    private BitMap cur;
    /**
     * 需要递增的位数的索引
     */
    private int[] incBit;
    /**
     * 当前正在递增的位数
     */
    private int diffBit;

    /**
     * 初始化
     *
     * @param start  起始的索引
     * @param end    终止的索引
     * @param  * @param start
 * @param end
 * @param incBit0
     */
    public SomeBitIncIdxIterator(int start, int end, BitSet incBit) {
        Tools.assert0(end >= start, "'end' must get 'start' ");
        Tools.assert0(incBit != null, "'incBit' can't be null ");

        this.end = BitMap.newUnsignedInt(end);
        this.cur = BitMap.newUnsignedInt(start, this.end.getCapacity());
        this.incBit = bitSet2IdxArr(incBit);
        diffBit = this.end.getCapacity() - 1;
    }

    // ----------------- 工具方法 -----------------------

    public static int incBit(BitMap cur, BitSet incBit) {
        return incBit(cur, bitSet2IdxArr(incBit));
    }

    public static int incBit(BitMap cur, int[] incBit) {
        int result = peek(cur);

        int lowerIncBit = cur.get(incBit[0]);
        if (lowerIncBit < 9) {
            cur.set(incBit[0], lowerIncBit + 1);
        } else {
            cur.set(incBit[0], 0);
            for (int i = 1; i < incBit.length; i++) {
                int curIncBit = cur.get(incBit[i]);
                if (curIncBit < 9) {
                    cur.set(incBit[i], curIncBit + 1);
                    break;
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
        for (int i = cur.getCapacity() - 1; i >= 0; i--) {
            result = radix * result + cur.get(i);
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
        if (diffBit < 0) {
            return false;
        }

        int endBit = end.get(diffBit), curBit = cur.get(diffBit);
        if (endBit > curBit) {
            return true;
        } else if (endBit < curBit) {
            return false;
            // incase of the 'diffBit' eq
        } else {
            for (int i = diffBit - 1; i >= 0; i--) {
                endBit = end.get(i);
                curBit = cur.get(i);
                if (endBit > curBit) {
                    diffBit = i;
                    return true;
                } else if (endBit < curBit) {
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
        if (!hasNext()) {
            throw new RuntimeException("have no next !");
        }

        return incBit(cur, incBit);
    }


    // ----------------- 辅助方法 -----------------------

    private static int[] bitSet2IdxArr(BitSet incBit) {
        int[] result = new int[incBit.cardinality()];
        int _start = -1, idx = 0;
        while ((_start = incBit.nextSetBit(_start + 1)) >= 0) {
            result[idx++] = _start;
        }
        return result;
    }

}
