/**
 * file name : HuffmanUtils.java
 * created at : ����3:19:30 2016��8��21��
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
 * ������������е�Ԫ�ص�HuffManCode
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
     * ���ݸ��������ȼ�����, ����huffmanTree
     * ���͵�̰���㷨ʵ��
     * ά��һ�����ȼ�����[����Node�Ĵ�С��������], ÿ�δӶ�����ȡ��С�������ڵ�, ���й鲢, ���鲢֮��Ľڵ�������
     * ���ϵĵ�����ȥ, ֱ�����е�Ԫ�� ���鲢��һ�����ڵ�����, ���ظ��ڵ�
     *
     * @param queue �����Ľڵ�����
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
     * ���ݹ���������huffmanTree, �ռ�����Ҷ�ӽ��
     *
     * @param node   constructHuffmanTree ��������ĸ��ڵ�
     * @param leaves Ŀ�꼯��, �����е�Ҷ�ӽڵ���ӵ���ǰ������
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

    // --------------------------- �������ݽṹ --------------------------------------

    /**
     * HuffMan���
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 5/4/2017 9:52 PM
     */
    public static class Node<T extends ComparableAndMergeable<T>> implements Comparable<Node<T>> {
        /**
         * ���ڵ���ַ�����ʾ
         */
        public static final String ROOT = "#";
        /**
         * ���ӵ�·�߱�ʾ
         */
        public static final String LEFT_CHILD = "0";
        /**
         * �Һ��ӵ�·�߱�ʾ
         */
        public static final String RIGHT_CHILD = "1";

        /**
         * ��ǰ�ڵ������
         */
        private T val;
        /**
         * ���ڵ�
         */
        Node<T> parent;
        /**
         * �����ӽڵ�
         */
        Node<T> left, right;
        /**
         * ��ǰ�ڵ��HuffManCode
         */
        String huffManCode;

        /**
         * ��ʼ��
         *
         * @param val   ��ǰ�ڵ������
         * @param left  ����
         * @param right �Һ���
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
         * ���㵱ǰNode��huffmanCode
         * ���Լ��򸸽ڵ㲻�ϵĻ���, Ȼ��������һ�»��ݵ�·��, ���ǵ�ǰ�ڵ��huffmanCode
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
         * ��ǰ�ڵ��Ǹ��ڵ������ �����Һ�����
         *
         * @param node ��ǰ�ڵ�
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
