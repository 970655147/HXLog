/**
 * file name : TreePattern.java
 * created at : ����8:29:35 2016��8��11��
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
 * ���ݸ��������νṹ, ������ӻ����ַ�����
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

    // add at 2016.07.06, ��ӡ����������״�ӿ�[�����ΪҶ�ڵ�, ���һ���ӽڵ�Ϊ��ǰ����Ԫ������Ϣ]
    // tree ��س���
    /**
     * �ڵ�����
     */
    public static final String TREE_NAME = "name";
    /**
     * �ڵ�����
     */
    public static final String TREE_TYPE = "type";
    /**
     * �ڵ�ռ�õĿռ�
     */
    public static final String TREE_SIZE = "size";

    /**
     * ����ڵ�[�ļ�]
     */
    public static final String TREE_OBJ = "obj";
    /**
     * ����ڵ�[�ļ���]
     */
    public static final String TREE_ARR = "arr";
    /**
     * ������ڵ�
     */
    public static final String TREE_NULL = "NULL";

    /**
     * һ���յ�List
     */
    private static final List<Integer> TREE_LIST_DUMMY = new ArrayList<>();
    /**
     * ����ƫ��
     */
    public static int TREE_OFFSET = 0;
    /**
     * ���������Ϣ֮����Ҫ��� ����ָ���������
     */
    public static int TREE_LENGTH_PER_SEP = 2;
    /**
     * ���������� û��Ԫ��, �Ƿ���Ҫ��� �س�
     */
    public static boolean TREE_IS_APPEND_CRLF_WHILE_NO_FILE = true;
    /**
     * ���������Ϣ֮��ķָ���
     */
    public static String TREE_SEPS = "-";
    /**
     * ��Ԫ�����׵�"����ָ���"
     */
    public static String TREE_VERTICAL_LINE = "|";

    /**
     * ��ȡTreeObj����Ϣ�ӿ�
     */
    public static final TreeObjInfoExtractor DEFAULT_TREE_OBJ_INFO_EXTRACTOR = new TreeObjInfoExtractor() {
        public String getTreeObjInfo(JSONObject obj) {
            return obj.getString(TREE_NAME) + "[" + Tools.getLengthString(obj.getLong(TREE_SIZE), Tools.KB) + "]";
        }
    };
    
    /**
     * ��ȡTreeArr����Ϣ�ӿ�
     */
    public static final TreeArrInfoExtractor DEFAULT_TREE_ARR_INFO_EXTRACTOR = new TreeArrInfoExtractor() {
        public String getTreeArrInfo(JSONObject obj) {
            return obj.getString(TREE_NAME) + "[dir]";
        }
    };


    /**
     * Լ��arr�ĵ�һ��Ԫ��Ϊ��ǰarr��Ԫ�ؾ�����
     *
     * @param arr                     ��ǰ���νṹ��JSONArray��ʾ[��һ��Ԫ��Ϊ��ǰ���νṹ��Ԫ����]
     * @param sb                      ��Ҫ�����StringBuilder
     * @param offset                  ��ǰ�㼶�Ļ���ƫ��
     * @param lengthPerSep            ���������Ϣ֮��, ��Ҫ����ָ���������
     * @param verticalLines           ��ǰ�㼶����"����ָ���"��λ��
     * @param isAppendCRLFWhileNoFile �����ǰArraryû�ж���, �Ƿ����һ��CRLF
     * @param objInfoExtractor        ��ȡ������Ϣ�ĺ���
     * @param arrInfoExtractor        ��ȡ������Ϣ�ĺ���
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
                    // ����˴���ӵ���ӵ�Ԫ�ظ�������1��, ���ӡһ���س� [����鿴]
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

    // ----------------- �������� -----------------------

    /**
     * ���Ƹ���������
     *
     * @param src ������ƫ������
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
     * ��sb�����length��"����ָ���"
     *
     * @param sb           ������StringBuilder
     * @param lengthPerSep ��Ҫ��ӵķָ���������
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
     * sb�е�һ��, ��offsets���ڵ��������"��ֱ����"
     * ���� offsets Ϊ [1, 3, 7], sb ��û������
     * ��ô��ǰ��������֮�� sb֮��Ϊ : _|_|___|
     * 0 -> 7
     *
     * @param sb      ������StringBuilder
     * @param offsets ��Ҫ��ӵ�
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
     * ��sb�����offset���ո�֮��, ���һ��"��ֱ������"
     *
     * @param sb     ������StringBuilder
     * @param offset ��Ҫ��ӿո������
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
     * ���ƫ�ƵĿո�
     *
     * @param sb     ������StringBuilder
     * @param offset ��Ҫ��ӵĿո������
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
