/**
 * file name : TreePattern.java
 * created at : 下午8:29:35 2016年8月11日
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
     * "空格"字符
     */
    public static String BLANK = " ";


    /**
     * 封装文件信息的接口
     * 提取文件的额所有信息
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
     * 提取 Tree节点 的信息接口
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
     * 根节点的值
     */
    public static Object ROOT_ID = null;

    /**
     * 生成给定目录的目录结构 [递归]
     * 对于文件获取数据 {type : typeVal, name : nameVal }
     * 对于目录获取数据 [{type : dir, name : curFolderName }, {... }, {... }, [... ], [... ] ]
     *
     * @param folder 给定的文件夹
     * @param result 给定的结果集合
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
     * 约定arr的第一个元素为当前arr的元素据描述
     *
     * @param arr                     当前树形结构的JSONArray表示[第一个元素为当前树形结构的元数据]
     * @param sb                      需要输出的StringBuilder
     * @param offset                  当前层级的基础偏移
     * @param lengthPerSep            输出基本信息之后, 需要输出分隔符的数量
     * @param verticalLines           当前层级各个"纵向分隔符"的位置
     * @param isAppendCRLFWhileNoFile 如果当前Arrary没有对象, 是否添加一个CRLF
     * @param nodeInfoExtractor        提取数组信息的函数
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
     * 根据所有的orgs 构建组织机构树
     * [{"id":1, "parentId":null }, {"id":2, "parentId":1 } ]
     * ||
     * \ /
     * {"id":1, "childs" : {"2" : {"id" : 2, "childs" : null } } }
     *
     * @param eles        给定的一系列的元素
     * @param putInfoFunc 将元素中的属性提取到目标JSONObject的方法
     * @param childsStr   子节点的key
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
     * 将给定的节点的childs的格式更新为数组
     * {id1 : {}, id2 : {} } -> [{id1}, {id2} ]
     *
     * @param obj 给定的JSONObject
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
            sb.append(BLANK);
        }
    }

    /**
     * 获取给定的文件夹的所有一级子文件家
     *
     * @param folder 给定的文件夹
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
     * 获取给定的文件夹的所有一级子文件
     *
     * @param folder 给定的文件夹
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
