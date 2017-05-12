/**
 * file name : TreePattern.java
 * created at : ����8:29:35 2016��8��11��
 * created by 970655147
 */

package com.hx.log.alogrithm.tree;

import com.hx.json.JSONArray;
import com.hx.json.JSONObject;
import com.hx.log.alogrithm.tree.interf.*;
import com.hx.log.util.Tools;

import java.io.File;
import java.io.FileFilter;
import java.util.*;

import static com.hx.log.util.Log.err;
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
     * "�ո�"�ַ�
     */
    public static String BLANK = " ";


    /**
     * ��װ�ļ���Ϣ�Ľӿ�
     * ��ȡ�ļ��Ķ�������Ϣ
     */
    public static final EncapFileInfo DEFAULT_ENCAP_FILE_INFO = new EncapFileInfo() {
        @Override
        public JSONObject encapFileInfo(File file, JSONObject result) {
            result.element("isDirectory", file.isDirectory()).element("canExecute", file.canExecute())
            .element("canRead", file.canRead()).element("canWrite", file.canWrite())
            .element("exists", file.exists()).element("absolutePath", file.getAbsolutePath())
            .element("freeSpace", file.getFreeSpace()).element("parent", file.getParent())
            .element("totalSpace", file.getTotalSpace()).element("freeSpace", file.getFreeSpace())
            .element("isAbsolute", file.isAbsolute()).element("isFile", file.isFile())
            .element("isHidden", file.isHidden()).element("lastModified", file.lastModified())
            .element("length", file.length()).element("toString", file.toString())
            .element(TREE_NAME, file.getName()).element(TREE_SIZE, file.length())
            .element(TREE_TYPE, file.isFile() ? TREE_OBJ : TREE_ARR)
            ;
            return result;
        }
    };

    /**
     * ��ȡ Tree�ڵ� ����Ϣ�ӿ�
     */
    public static final NodeInfoExtractor DEFAULT_NODE_INFO_EXTRACTOR = new NodeInfoExtractor() {
        @Override
        public String getTreeObjInfo(JSONObject obj) {
            return obj.getString(TREE_NAME) + "[" + Tools.getLengthString(obj.getLong(TREE_SIZE), Tools.KB) + "]";
        }

        public String getTreeArrInfo(JSONObject obj) {
            return obj.getString(TREE_NAME) + "[dir]";
        }
    };

    /**
     * childs
     */
    public static String CHILDS_STR = "childs";
    /**
     * ���ڵ��ֵ
     */
    public static Object ROOT_ID = null;

    /**
     * ���ɸ���Ŀ¼��Ŀ¼�ṹ [�ݹ�]
     * �����ļ���ȡ���� {type : typeVal, name : nameVal }
     * ����Ŀ¼��ȡ���� [{type : dir, name : curFolderName }, {... }, {... }, [... ], [... ] ]
     *
     * @param folder �������ļ���
     * @param result �����Ľ������
     * @return void
     * @author Jerry.X.He
     * @date 5/11/2017 9:47 PM
     * @since 1.0
     */
    public static void generateDirectorStructure(File folder, JSONArray result, EncapFileInfo encap) {
        Tools.assert0(folder != null, "'folder' can't be null !");
        if (!folder.exists()) {
            err("please makeSure " + folder.getPath() + " does exists ...");
            return;
        }

        if (folder.isDirectory()) {
            result.add(encap.encapFileInfo(folder, new JSONObject()));

            File[] files = getFiles(folder);
            for (File file : files) {
                result.add(encap.encapFileInfo(file, new JSONObject()));
            }

            File[] dirs = getFolders(folder);
            for (File dir : dirs) {
                JSONArray arr = new JSONArray();
                generateDirectorStructure(dir, arr, encap);
                result.add(arr);
            }
        }
    }

    public static void generateDirectorStructure(File folder, JSONArray result) {
        generateDirectorStructure(folder, result, DEFAULT_ENCAP_FILE_INFO);
    }

    public static void generateDirectorStructure(String folder, JSONArray result, EncapFileInfo encap) {
        Tools.assert0(folder != null, "'folder' can't be null !");
        generateDirectorStructure(new File(folder), result, encap);
    }

    public static void generateDirectorStructure(String folder, JSONArray result) {
        generateDirectorStructure(folder, result, DEFAULT_ENCAP_FILE_INFO);
    }

    /**
     * Լ��arr�ĵ�һ��Ԫ��Ϊ��ǰarr��Ԫ�ؾ�����
     *
     * @param arr                     ��ǰ���νṹ��JSONArray��ʾ[��һ��Ԫ��Ϊ��ǰ���νṹ��Ԫ����]
     * @param sb                      ��Ҫ�����StringBuilder
     * @param offset                  ��ǰ�㼶�Ļ���ƫ��
     * @param lengthPerSep            ���������Ϣ֮��, ��Ҫ����ָ���������
     * @param verticalLines           ��ǰ�㼶����"����ָ���"��λ��
     * @param isAppendCRLFWhileNoFile �����ǰArraryû�ж���, �Ƿ����һ��CRLF
     * @param nodeInfoExtractor        ��ȡ������Ϣ�ĺ���
     * @return int
     * @author Jerry.X.He
     * @date 5/4/2017 10:17 PM
     * @since 1.0
     */
    public static int tree(JSONArray arr, StringBuilder sb, int offset, int lengthPerSep,
                           List<Integer> verticalLines, boolean isAppendCRLFWhileNoFile,
                           NodeInfoExtractor nodeInfoExtractor) {
        assert0(arr != null, "'arr' can't be null ");
        assert0(sb != null, "'sb' can't be null ");
        assert0(nodeInfoExtractor != null, "'nodeInfoExtractor' can't be null ");

        JSONObject meta = arr.getJSONObject(0);
        int rows = 0;
        int appendedRows = 0;

        if (Tools.equalsIgnoreCase(meta.optString(TREE_TYPE), TREE_ARR)) {
            String folerInfo = nodeInfoExtractor.getTreeArrInfo(meta);
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
                    sb.append(nodeInfoExtractor.getTreeObjInfo(subObj));
                    rows++;
                } else {
                    JSONArray subArr = arr.getJSONArray(i);
                    sb.append(Tools.CRLF);
                    appendVerticalLine(sb, newVerticalLines);
                    appendSeps(sb, lengthPerSep);
                    appendedRows = tree(subArr, sb, (verticalLineOffset + lengthPerSep), lengthPerSep,
                            copyOfList0(newVerticalLines), isAppendCRLFWhileNoFile, nodeInfoExtractor);
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
                DEFAULT_NODE_INFO_EXTRACTOR);
        return sb.toString();
    }

    public static String tree(JSONArray arr, NodeInfoExtractor arrInfoExtractor) {
        StringBuilder sb = new StringBuilder();
        tree(arr, sb, TREE_OFFSET, TREE_LENGTH_PER_SEP, TREE_LIST_DUMMY, TREE_IS_APPEND_CRLF_WHILE_NO_FILE,
                arrInfoExtractor);
        return sb.toString();
    }

    public static String tree(JSONArray arr) {
        return tree(arr, DEFAULT_NODE_INFO_EXTRACTOR);
    }

    /**
     * �������е�orgs ������֯������
     * [{"id":1, "parentId":null }, {"id":2, "parentId":1 } ]
     * ||
     * \ /
     * {"id":1, "childs" : {"2" : {"id" : 2, "childs" : null } } }
     *
     * @param eles        ������һϵ�е�Ԫ��
     * @param putInfoFunc ��Ԫ���е�������ȡ��Ŀ��JSONObject�ķ���
     * @param childsStr   �ӽڵ��key
     * @param rootId      rootId
     * @return com.hx.json.JSONObject
     * @author Jerry.X.He
     * @date 5/5/2017 9:33 PM
     * @since 1.0
     */
    public static <T extends TreeIdExtractor<T, IdType>, IdType> JSONObject generateTree(List<T> eles,
                                                                                         TreeInfoExtractor<T> putInfoFunc,
                                                                                         String childsStr, IdType rootId) {
        JSONObject root = new JSONObject();
        Map<IdType, JSONObject> id2AreaObj = new HashMap<>();
        for (T ele : eles) {
            IdType id = ele.id();
            IdType parentId = ele.parentId();
            for (IdType areaId : Arrays.asList(id, parentId)) {
                if (!id2AreaObj.containsKey(areaId)) {
                    JSONObject areaObj = new JSONObject();
                    areaObj.put(childsStr, new JSONObject());
                    id2AreaObj.put(areaId, areaObj);
                }
            }

            putInfoFunc.extract(ele, id2AreaObj.get(id));
            JSONObject parentObj = id2AreaObj.get(parentId);
            JSONObject childObjs = parentObj.getJSONObject(childsStr);
            childObjs.put(String.valueOf(id), id2AreaObj.get(id));

            if (Objects.equals(parentId, rootId)) {
                root = id2AreaObj.get(id);
            }
        }

        return root;
    }

    public static <T extends TreeIdExtractor<T, IdType>, IdType> JSONObject generateTree(List<T> eles,
                                                                                         TreeInfoExtractor<T> putInfoFunc) {
        return generateTree(eles, putInfoFunc, CHILDS_STR, (IdType) ROOT_ID);
    }

    /**
     * �������Ľڵ��childs�ĸ�ʽ����Ϊ����
     * {id1 : {}, id2 : {} } -> [{id1}, {id2} ]
     *
     * @param obj ������JSONObject
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 10:02 PM
     * @since 1.0
     */
    public static JSONObject childArrayify(JSONObject obj, String childsStr) {
        JSONArray newChilds = new JSONArray();
        JSONObject childs = obj.getJSONObject(CHILDS_STR);
        if (childs != null) {
            for (Map.Entry<String, Object> entry : childs.entrySet()) {
                JSONObject valObj = (JSONObject) (entry.getValue());
                newChilds.add(valObj);
                if (!valObj.isEmpty()) {
                    childArrayify(valObj);
                }
            }
            obj.put(CHILDS_STR, newChilds);
        }

        return obj;
    }

    public static JSONObject childArrayify(JSONObject obj) {
        return childArrayify(obj, CHILDS_STR);
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
            sb.append(BLANK);
        }
    }

    /**
     * ��ȡ�������ļ��е�����һ�����ļ���
     *
     * @param folder �������ļ���
     * @return java.io.File[]
     * @author Jerry.X.He
     * @date 5/11/2017 9:48 PM
     * @since 1.0
     */
    private static File[] getFolders(File folder) {
        return folder.listFiles(new FileFilter() {
            public boolean accept(File file) {
                return file.isDirectory();
            }
        });
    }

    /**
     * ��ȡ�������ļ��е�����һ�����ļ�
     *
     * @param folder �������ļ���
     * @return java.io.File[]
     * @author Jerry.X.He
     * @date 5/11/2017 9:48 PM
     * @since 1.0
     */
    private static File[] getFiles(File folder) {
        return folder.listFiles(new FileFilter() {
            public boolean accept(File file) {
                return file.isFile();
            }
        });
    }

}
