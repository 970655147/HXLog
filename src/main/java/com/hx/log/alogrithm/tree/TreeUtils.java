/**
 * file name : TreePattern.java
 * created at : 下午8:29:35 2016年8月11日
 * created by 970655147
 */

package com.hx.log.alogrithm.tree;

import com.hx.json.JSONArray;
import com.hx.json.JSONObject;
import com.hx.log.alogrithm.tree.interf.TreeArrInfoExtractor;
import com.hx.log.alogrithm.tree.interf.TreeObjInfoExtractor;
import com.hx.log.util.Tools;

import java.util.ArrayList;
import java.util.List;

import static com.hx.log.util.Tools.assert0;

/**
 * 根据给定的树形结构, 输出可视化的字符序列
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 10:06 PM
 */
public final class TreeUtils {

    // disable constructor
    private TreeUtils() {
        assert0("can't instantiate !");
    }

    // add at 2016.07.06, 打印出给定的树状接口[如果不为叶节点, 则第一个子节点为当前结点的元数据信息]
    // tree 相关常量
    /**
     * 节点名称
     */
    public static final String TREE_NAME = "name";
    /**
     * 节点类型
     */
    public static final String TREE_TYPE = "type";
    /**
     * 节点占用的空间
     */
    public static final String TREE_SIZE = "size";

    /**
     * 对象节点[文件]
     */
    public static final String TREE_OBJ = "obj";
    /**
     * 数组节点[文件夹]
     */
    public static final String TREE_ARR = "arr";
    /**
     * 空数组节点
     */
    public static final String TREE_NULL = "NULL";

    /**
     * 一个空的List
     */
    private static final List<Integer> TREE_LIST_DUMMY = new ArrayList<>();
    /**
     * 基础偏移
     */
    public static int TREE_OFFSET = 0;
    /**
     * 输出对象信息之后需要输出 横向分隔符的数量
     */
    public static int TREE_LENGTH_PER_SEP = 2;
    /**
     * 如果数组对象 没有元素, 是否需要输出 回车
     */
    public static boolean TREE_IS_APPEND_CRLF_WHILE_NO_FILE = true;
    /**
     * 输出对象信息之后的分隔符
     */
    public static String TREE_SEPS = "-";
    /**
     * 子元素行首的"纵向分隔符"
     */
    public static String TREE_VERTICAL_LINE = "|";

    /**
     * 提取TreeObj的信息接口
     */
    public static final TreeObjInfoExtractor DEFAULT_TREE_OBJ_INFO_EXTRACTOR = new TreeObjInfoExtractor() {
        public String getTreeObjInfo(JSONObject obj) {
            return obj.getString(TREE_NAME) + "[" + Tools.getLengthString(obj.getLong(TREE_SIZE), Tools.KB) + "]";
        }
    };
    
    /**
     * 提取TreeArr的信息接口
     */
    public static final TreeArrInfoExtractor DEFAULT_TREE_ARR_INFO_EXTRACTOR = new TreeArrInfoExtractor() {
        public String getTreeArrInfo(JSONObject obj) {
            return obj.getString(TREE_NAME) + "[dir]";
        }
    };


    /**
     * 约定arr的第一个元素为当前arr的元素据描述
     *
     * @param arr                     当前树形结构的JSONArray表示[第一个元素为当前树形结构的元数据]
     * @param sb                      需要输出的StringBuilder
     * @param offset                  当前层级的基础偏移
     * @param lengthPerSep            输出基本信息之后, 需要输出分隔符的数量
     * @param verticalLines           当前层级各个"纵向分隔符"的位置
     * @param isAppendCRLFWhileNoFile 如果当前Arrary没有对象, 是否添加一个CRLF
     * @param objInfoExtractor        提取对象信息的函数
     * @param arrInfoExtractor        提取数组信息的函数
     * @return int
     * @author Jerry.X.He
     * @date 5/4/2017 10:17 PM
     * @since 1.0
     */
    public static int tree(JSONArray arr, StringBuilder sb, int offset, int lengthPerSep,
                           List<Integer> verticalLines, boolean isAppendCRLFWhileNoFile,
                           TreeObjInfoExtractor objInfoExtractor, TreeArrInfoExtractor arrInfoExtractor) {
        assert0(arr != null, "'arr' can't be null ");
        assert0(sb != null, "'sb' can't be null ");
        assert0(objInfoExtractor != null, "'objInfoExtractor' can't be null ");
        assert0(arrInfoExtractor != null, "'arrInfoExtractor' can't be null ");

        JSONObject meta = arr.getJSONObject(0);
        int rows = 0;
        int appendedRows = 0;

        if (Tools.equalsIgnoreCase(meta.getString(TREE_TYPE), TREE_ARR)) {
            String folerInfo = arrInfoExtractor.getTreeArrInfo(meta);
            sb.append(folerInfo);
            appendSeps(sb, lengthPerSep + 1);
            int verticalLineOffset = offset + folerInfo.length() + lengthPerSep;

            List<Integer> newVerticalLines = copyOfList0(verticalLines);
            newVerticalLines.add(verticalLineOffset);
            for (int i = 1, len = arr.size(); i < len; i++) {
                JSONObject subObj = arr.optJSONObject(i);
                // switch of 'Obj' or 'Arr'
                if (subObj != null) {
                    if (i != 1) {
                        sb.append(Tools.CRLF);
                        appendVerticalLine(sb, newVerticalLines);
                    }
                    appendSeps(sb, lengthPerSep);
                    sb.append(objInfoExtractor.getTreeObjInfo(subObj));
                    rows++;
                } else {
                    JSONArray subArr = arr.getJSONArray(i);
                    sb.append(Tools.CRLF);
                    appendVerticalLine(sb, newVerticalLines);
                    appendSeps(sb, lengthPerSep);
                    appendedRows = tree(subArr, sb, (verticalLineOffset + lengthPerSep), lengthPerSep,
                            copyOfList0(newVerticalLines), isAppendCRLFWhileNoFile, objInfoExtractor, arrInfoExtractor);
                    // 如果此次添加的添加的元素个数大于1个, 则打印一个回车 [方便查看]
                    rows += appendedRows;
                }
            }
        }

        if (rows == 0) {
            appendSeps(sb, lengthPerSep + 1);
            Tools.append(sb, TREE_NULL);
        }
        if (isAppendCRLFWhileNoFile) {
            if (rows - appendedRows > 0) {
                Tools.append(sb, Tools.CRLF);
                appendVerticalLine(sb, verticalLines);
            }
        }

        return rows;
    }

    public static String tree(JSONArray arr, int offset, int lengthPerSep, boolean isAppendCRLFWhileNoFile) {
        StringBuilder sb = new StringBuilder();
        tree(arr, sb, offset, lengthPerSep, TREE_LIST_DUMMY, isAppendCRLFWhileNoFile,
                DEFAULT_TREE_OBJ_INFO_EXTRACTOR, DEFAULT_TREE_ARR_INFO_EXTRACTOR);
        return sb.toString();
    }

    public static String tree(JSONArray arr, TreeObjInfoExtractor objInfoExtractor, TreeArrInfoExtractor arrInfoExtractor) {
        StringBuilder sb = new StringBuilder();
        tree(arr, sb, TREE_OFFSET, TREE_LENGTH_PER_SEP, TREE_LIST_DUMMY, TREE_IS_APPEND_CRLF_WHILE_NO_FILE,
                objInfoExtractor, arrInfoExtractor);
        return sb.toString();
    }

    public static String tree(JSONArray arr) {
        return tree(arr, DEFAULT_TREE_OBJ_INFO_EXTRACTOR, DEFAULT_TREE_ARR_INFO_EXTRACTOR);
    }

    // ----------------- 辅助方法 -----------------------

    /**
     * 复制给定的数组
     *
     * @param src 给定的偏移数组
     * @return java.util.List<java.lang.Integer>
     * @author Jerry.X.He
     * @date 5/4/2017 10:10 PM
     * @since 1.0
     */
    private static List<Integer> copyOfList0(List<Integer> src) {
        List<Integer> result = new ArrayList<>(src.size());
        result.addAll(src);
        return result;
    }

    /**
     * 向sb中添加length个"横向分隔符"
     *
     * @param sb           给定的StringBuilder
     * @param lengthPerSep 需要添加的分隔符的数量
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 10:10 PM
     * @since 1.0
     */
    private static void appendSeps(StringBuilder sb, int lengthPerSep) {
        for (int i = 0; i < lengthPerSep; i++) {
            sb.append(TREE_SEPS);
        }
    }

    /**
     * sb中的一行, 在offsets所在的坐标添加"垂直竖线"
     * 比如 offsets 为 [1, 3, 7], sb 中没有数据
     * 那么当前方法处理之后 sb之后为 : _|_|___|
     * 0 -> 7
     *
     * @param sb      给定的StringBuilder
     * @param offsets 需要添加的
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 10:13 PM
     * @since 1.0
     */
    private static void appendVerticalLine(StringBuilder sb, List<Integer> offsets) {
        int last = 0;
        for (Integer off : offsets) {
            appendOffset(sb, off - last);
            sb.append(TREE_VERTICAL_LINE);
            last = off;
        }
    }

    /**
     * 向sb中添加offset个空格之后, 添加一个"垂直的竖线"
     *
     * @param sb     给定的StringBuilder
     * @param offset 需要添加空格的数量
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 10:11 PM
     * @since 1.0
     */
    private static void appendVerticalLine(StringBuilder sb, int offset) {
        appendOffset(sb, offset);
        sb.append(TREE_VERTICAL_LINE);
    }

    /**
     * 添加偏移的空格
     *
     * @param sb     给定的StringBuilder
     * @param offset 需要添加的空格的数量
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 10:12 PM
     * @since 1.0
     */
    private static void appendOffset(StringBuilder sb, int offset) {
        for (int i = 0; i < offset; i++) {
            sb.append(" ");
        }
    }

}
