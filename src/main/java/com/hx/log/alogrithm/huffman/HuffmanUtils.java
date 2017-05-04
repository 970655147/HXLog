/**
 * file name : HuffmanUtils.java
 * created at : 下午3:19:30 2016年8月21日
 * created by 970655147
 */

package com.hx.log.alogrithm.huffman;

import com.hx.common.math.GeometryUtils;
import com.hx.json.JSONObject;
import com.hx.log.alogrithm.huffman.interf.ComparableAndMergeable;
import com.hx.log.util.Tools;

import java.util.List;
import java.util.PriorityQueue;

import static com.hx.log.util.Tools.assert0;

/**
 * 计算给定的序列的元素的HuffManCode
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 9:48 PM
 */
public final class HuffmanUtils {

    // disable constructor
    private HuffmanUtils() {
        assert0("can't instantiate !");
    }

    /**
     * 根据给定的优先级队列, 构造huffmanTree
     * 典型的贪心算法实现
     * 维护一个优先级队列[根据Node的大小进行排序], 每次从队列中取最小的两个节点, 进行归并, 将归并之后的节点放入队列
     * 不断的迭代下去, 直到所有的元素 被归并到一个根节点下面, 返回根节点
     *
     * @param queue 给定的节点序列
     * @return com.hx.log.alogrithm.huffman.HuffmanUtils.Node<T>
     * @author Jerry.X.He
     * @date 5/4/2017 9:49 PM
     * @since 1.0
     */
    public static <T extends ComparableAndMergeable<T>> Node<T> constructHuffmanTree(PriorityQueue<Node<T>> queue) {
        assert0(queue != null, "'queue' can't be null !");

        while (queue.size() > 1) {
            Node<T> smaller = queue.remove();
            Node<T> bigger = queue.remove();
            Node<T> merged = new Node<T>(smaller.val.merge(bigger.val), smaller, bigger);
            smaller.setParent(merged);
            bigger.setParent(merged);
            queue.add(merged);
        }
        return queue.remove();
    }

    /**
     * 根据构建出来的huffmanTree, 收集各个叶子结点
     *
     * @param node   constructHuffmanTree 构造出来的根节点
     * @param leaves 目标集合, 将所有的叶子节点添加到当前集合中
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 9:51 PM
     * @since 1.0
     */
    public static <T extends ComparableAndMergeable<T>> void collectLeaveNodes(Node<T> node, List<Node<T>> leaves) {
        if (node == null) {
            return;
        }

        if (node.left != null) {
            collectLeaveNodes(node.left, leaves);
        }
        if (node.right != null) {
            collectLeaveNodes(node.right, leaves);
        }
        if ((node.left == null) && (node.right == null)) {
            leaves.add(node);
        }
    }

    // --------------------------- 辅助数据结构 --------------------------------------

    /**
     * HuffMan结点
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 5/4/2017 9:52 PM
     */
    public static class Node<T extends ComparableAndMergeable<T>> implements Comparable<Node<T>> {
        /**
         * 根节点的字符串表示
         */
        public static final String ROOT = "#";
        /**
         * 左孩子的路线表示
         */
        public static final String LEFT_CHILD = "0";
        /**
         * 右孩子的路线表示
         */
        public static final String RIGHT_CHILD = "1";

        /**
         * 当前节点的数据
         */
        private T val;
        /**
         * 父节点
         */
        Node<T> parent;
        /**
         * 左右子节点
         */
        Node<T> left, right;
        /**
         * 当前节点的HuffManCode
         */
        String huffManCode;

        /**
         * 初始化
         *
         * @param val   当前节点的数据
         * @param left  左孩子
         * @param right 右孩子
         * @since 1.0
         */
        public Node(T val, Node<T> left, Node<T> right) {
            this.val = val;
            this.left = left;
            this.right = right;
        }

        public Node() {
            super();
        }

        /**
         * setter & getter
         */
        public void setParent(Node<T> parent) {
            this.parent = parent;
        }

        public T getVal() {
            return val;
        }

        public String getHuffManCode() {
            if (this.huffManCode == null) {
                huffManCode = calcHuffManCode();
            }

            return huffManCode;
        }

        /**
         * for TreeSet, for priorityQueue
         */
        public int compareTo(Node<T> o) {
            return this.val.compareTo(o.val);
        }

        /**
         * 计算当前Node的huffmanCode
         * 从自己向父节点不断的回溯, 然后在逆序一下回溯的路线, 即是当前节点的huffmanCode
         *
         * @return java.lang.String
         * @author Jerry.X.He
         * @date 5/4/2017 9:55 PM
         * @since 1.0
         */
        private String calcHuffManCode() {
            StringBuilder sb = new StringBuilder();
            Node<T> node = this;

            while (node.parent != null) {
                sb.append(isLeftOrRight(node));
                node = node.parent;
            }

            return sb.reverse().toString();
        }

        /**
         * 当前节点是父节点的左孩子 还是右孩子呢
         *
         * @param node 当前节点
         * @return java.lang.String
         * @author Jerry.X.He
         * @date 5/4/2017 9:56 PM
         * @since 1.0
         */
        private String isLeftOrRight(Node<T> node) {
            if (node == null) {
                return Tools.NULL;
            }
            Node<T> parent = node.parent;
            if (parent == null) {
                return ROOT;
            }

            if (parent.left == node) {
                return LEFT_CHILD;
            } else {
                return RIGHT_CHILD;
            }
        }

        /**
         * for debug ...
         *
         * @return java.lang.String
         * @author Jerry.X.He
         * @date 5/4/2017 9:58 PM
         * @since 1.0
         */
        public String toString() {
            return new JSONObject()
                    .element("val", val).element("left", left).element("right", right)
                    .toString();
        }
    }


}
