package com.hx.log.idx.idx_iterator;

import com.hx.common.interf.idx.IdxIterator;

import java.util.Collection;
import java.util.PriorityQueue;

/**
 * 维护顺序的 IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 6/8/2017 7:34 PM
 */
public class SortedIdxIterator implements IdxIterator {

    /**
     * 维护优先级队列
     */
    private PriorityQueue<Integer> priorityQueue;

    public SortedIdxIterator(Collection<Integer> col) {
        this();
        priorityQueue.addAll(col);
    }

    public SortedIdxIterator() {
        priorityQueue = new PriorityQueue<>();
    }

    public void add(Integer newEle) {
        priorityQueue.add(newEle);
    }

    @Override
    public boolean hasNext() {
        return ! priorityQueue.isEmpty();
    }

    @Override
    public int next() {
        return priorityQueue.poll();
    }

    @Override
    public IdxIterator copy() {
        SortedIdxIterator result = new SortedIdxIterator();
        for(Integer ele : priorityQueue) {
            result.add(ele);
        }

        return result;
    }
}
