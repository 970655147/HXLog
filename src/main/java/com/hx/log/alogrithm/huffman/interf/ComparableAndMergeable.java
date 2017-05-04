package com.hx.log.alogrithm.huffman.interf;

/**
 * 可比较, 并且是可归并的
 * 约定的给定的数据结构需要实现的compareTo, merge接口
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 9:58 PM
 */
public interface ComparableAndMergeable<T> {

    /**
     * 当前对象 和给定的对象相比, >0 表示当前对象 > 给定的对象, =0 表示当前对象 = 给定的对象, <0 表示当前对象 < 给定的对象
     *
     * @param other 需要比较的对象
     * @return >0 represent currentObject > other, =0 represent currentObj = other, or else currentObj < other
     * @author Jerry.X.He
     * @date 5/4/2017 9:59 PM
     * @since 1.0
     */
    int compareTo(T other);

    /**
     * 归并当前对象 和other
     *
     * @param other 需要归并的对象
     * @return the ComparableAndMergeable after merged
     * @author Jerry.X.He
     * @date 5/4/2017 10:00 PM
     * @since 1.0
     */
    T merge(T other);

}
