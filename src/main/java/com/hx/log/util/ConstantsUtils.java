/**
 * file name : ConstantsUtils.java
 * created at : 下午4:59:23 2016年6月25日
 * created by 970655147
 */

package com.hx.log.util;

import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;

public final class ConstantsUtils {

    // disable constructor
    private ConstantsUtils() {
        Tools.assert0("can't instantiate !");
    }

    // 相关常量
    public static String SEP = "=";
    public static String DEFAULT_PROPS = "DEFAULT_PROPS";
    public static String PROPS = "PROPS";
    public static String UTILS = "Tools";

    // 标记各个type
    public static final int withStaticFields = 0;
    public static final int withOpt = withStaticFields + 1;

    /**
     * @param configPath 配置文件的路径
     * @return
     * @throws IOException
     * @Name: generateConstants
     * @Description: 根据给定的相关配置文件中的信息, 生成应该属于Constants中的内容[我这里 仅仅以我的偏好来写]
     * @Create at 2016年6月25日 下午5:32:49 by '970655147'
     */
    public static String generateCodes0(String configPath, int typeIdx) throws Exception {
        List<String> lines = Tools.getContentWithList(configPath, 100);
        Map<String, String> prop = new LinkedHashMap<>();
        Set<Integer> appendCRLF = new HashSet<>();

        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            if (isEmptyOrComment(line)) {
                continue;
            }

            String[] splits = line.split(SEP);
            Tools.assert0(splits.length >= 2, "not good format : " + line);
            prop.put(splits[0].trim(), splits[1].trim());
            if (isEmptyOrComment(lines.get(i + 1))) {
                appendCRLF.add(prop.size());
            }
        }

        String codes = null;
        if (typeIdx == withStaticFields) {
            codes = generateCodesWithStaticFields(prop, appendCRLF);
        } else if (typeIdx == withOpt) {
            codes = generateCodesWithOpt(prop, appendCRLF);
        } else {
            Tools.assert0("have no type with : " + typeIdx);
        }

        return codes;
    }

    /**
     * @param configPath 配置文件的路径
     * @return
     * @throws Exception
     * @Name: generateCodesWithStaticFields
     * @Description: 根据给定的配置文件生成对应的Constants代码
     * public static final DEFAULT_IP = "localhost";
     * // ....
     * public final _IP = DEFAULT_IP;
     * // ....
     * public static void load(Map<String, String> PROPS) {
     * _MONGO_IP = Tools.optString(PROPS, "ip", DEFAULT_IP);
     * // ....
     * }
     * @Create at 2016年6月28日 下午9:18:17 by '970655147'
     */
    public static String generateCodesWithStaticFields(String configPath) throws Exception {
        return generateCodes0(configPath, withStaticFields);
    }

    public static String generateCodesWithStaticFields(Map<String, String> prop, Set<Integer> appendCRLF) {
        StringBuilder sb = new StringBuilder();
        List<String> types = new ArrayList<>(prop.size());

        int idx = 0;
        // public static final String defaultXXX = xx
        for (Entry<String, String> entry : prop.entrySet()) {
            String type = confirmType(entry.getValue());
            types.add(type);
            if (STRING == type) {
                Tools.appendCRLF(sb, "public static final " + type + " " + getDefaultAttrName(entry.getKey()) + " = \"" + entry.getValue() + "\";");
            } else {
                Tools.appendCRLF(sb, "public static final " + type + " " + getDefaultAttrName(entry.getKey()) + " = " + entry.getValue() + ";");
            }

            idx++;
            if (appendCRLF.contains(idx)) {
                Tools.appendCRLF(sb, Tools.EMPTY_STR);
            }
        }
        Tools.appendCRLF(sb, Tools.EMPTY_STR);

        idx = 0;
        // static String XXX = xx
        for (Entry<String, String> entry : prop.entrySet()) {
            String type = confirmType(entry.getValue());
            Tools.appendCRLF(sb, "static " + type + " " + getAttrName(entry.getKey()) + " = " + getDefaultAttrName(entry.getKey()) + ";");

            idx++;
            if (appendCRLF.contains(idx)) {
                Tools.appendCRLF(sb, Tools.EMPTY_STR);
            }
        }
        Tools.appendCRLF(sb, Tools.EMPTY_STR);

        idx = 0;
        // XXX = props.getProperty('key', xx);
        Tools.appendCRLF(sb, "public static void load(Map<String, String> " + PROPS + ") { ");
        for (Entry<String, String> entry : prop.entrySet()) {
            String type = confirmType(entry.getValue());
            if (STRING.equals(types.get(idx))) {
                Tools.appendCRLF(sb, "	" + getAttrName(entry.getKey()) + " = " + UTILS + ".optString(" + PROPS + ", \"" + entry.getKey() + "\", " + getDefaultAttrName(entry.getKey()) + ");");
            } else {
                if (INTEGER.equals(types.get(idx))) {
                    Tools.appendCRLF(sb, "	" + getAttrName(entry.getKey()) + " = " + UTILS + ".optInt(" + PROPS + ", \"" + entry.getKey() + "\", " + getDefaultAttrName(entry.getKey()) + ");");
                } else if (BOOLEAN.equals(types.get(idx))) {
                    Tools.appendCRLF(sb, "	" + getAttrName(entry.getKey()) + " = " + UTILS + ".optBoolean(" + PROPS + ", \"" + entry.getKey() + "\", " + getDefaultAttrName(entry.getKey()) + ");");
                } else if (FLOAT.equals(types.get(idx))) {
                    Tools.appendCRLF(sb, "	" + getAttrName(entry.getKey()) + " = " + UTILS + ".optFloat(" + PROPS + ", \"" + entry.getKey() + "\", " + getDefaultAttrName(entry.getKey()) + ");");
                }
            }

            idx++;
            if (appendCRLF.contains(idx)) {
                Tools.appendCRLF(sb, Tools.EMPTY_STR);
            }
        }
        Tools.appendCRLF(sb, "}");
        Tools.appendCRLF(sb, Tools.EMPTY_STR);

        return sb.toString();
    }

    /**
     * @param configPath 配置文件的路径
     * @return
     * @throws Exception
     * @Name: generateCodesWithOpt
     * @Description: 根据给定的配置文件生成对应的Constants代码
     * public static final String _MONGO_IP = "mongoIp";
     * // ....
     * public static final Map<String, String> DEFAULT_PROPS = new HashMap<>();
     * static {
     * DEFAULT_PROPS.put(_MONGO_IP, "localhost");
     * // ....
     * }
     * public static String optString(String key, String defaultVal) {}
     * public static int optInt(String key, int defaultVal) {}
     * // ....
     * @Create at 2016年6月28日 下午9:18:41 by '970655147'
     */
    public static String generateCodesWithOpt(String configPath) throws Exception {
        return generateCodes0(configPath, withOpt);
    }

    public static String generateCodesWithOpt(Map<String, String> config, Set<Integer> appendCRLF) {
        StringBuilder sb = new StringBuilder();

        int idx = 0;
        Tools.appendCRLF(sb, "// 配置相关常量 ");
        for (Map.Entry<String, String> entry : config.entrySet()) {
            Tools.appendCRLF(sb, "public static final String " + getAttrName(entry.getKey()) + " = \"" + entry.getKey() + "\";");

            idx++;
            if (appendCRLF.contains(idx)) {
                Tools.appendCRLF(sb, Tools.EMPTY_STR);
            }
        }

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "// 默认的配置 ");
        Tools.appendCRLF(sb, "public static final Map<String, String> " + DEFAULT_PROPS + " = new HashMap<>(); ");
        Tools.appendCRLF(sb, "static {");
        idx = 0;
        for (Map.Entry<String, String> entry : config.entrySet()) {
            // 一定要替换\r\n, 不能仅仅使用\n
            String val = entry.getValue().replace("\r\n", "\\r\\n");
            Tools.appendCRLF(sb, "	" + DEFAULT_PROPS + ".put(" + getAttrName(entry.getKey()) + ", \"" + val + "\"); ");

            idx++;
            if (appendCRLF.contains(idx)) {
                Tools.appendCRLF(sb, Tools.EMPTY_STR);
            }
        }
        Tools.appendCRLF(sb, "}");

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "// 获取相关默认值");
        Tools.appendCRLF(sb, "" +
                "public static String optString(String key, String defaultVal) {\n" +
                "String val = (props != null) ? " + PROPS + ".get(key) : null;\n" +
                "\tval = (val != null) ? val : " + DEFAULT_PROPS + ".get(key);\n" +
                "\n" +
                "\tif(val == null) {\n" +
                "\t\treturn defaultVal;\n" +
                "\t}\n" +
                "\treturn val;\n" +
                "}\n" +
                "public static int optInt(String key, int defaultVal) {\n" +
                "\tString val = optString(key);\n" +
                "\n" +
                "\tif(isEmpty0(val) ) {\n" +
                "\t\treturn defaultVal;\n" +
                "\t}\n" +
                "\treturn Integer.parseInt(val);\n" +
                "}\n" +
                "public static long optLong(String key, long defaultVal) {\n" +
                "\tString val = optString(key);\n" +
                "\n" +
                "\tif(isEmpty0(val) ) {\n" +
                "\t\treturn defaultVal;\n" +
                "\t}\n" +
                "\treturn Long.parseLong(val);\n" +
                "}\n" +
                "public static boolean optBoolean(String key, boolean defaultVal) {\n" +
                "\tString val = optString(key);\n" +
                "\n" +
                "\tif(isEmpty0(val) ) {\n" +
                "\t\treturn defaultVal;\n" +
                "\t}\n" +
                "\treturn Boolean.parseBoolean(val);\n" +
                "}\n" +
                "public static double optDouble(String key, double defaultVal) {\n" +
                "\tString val = optString(key);\n" +
                "\n" +
                "\tif(isEmpty0(val) ) {\n" +
                "\t\treturn defaultVal;\n" +
                "\t}\n" +
                "\treturn Double.parseDouble(val);\n" +
                "}\n" +
                "\n" +
                "public static String optString(String key) {\n" +
                "\treturn optString(key, null);\n" +
                "}\n" +
                "public static int optInt(String key) {\n" +
                "\treturn optInt(key, 0);\n" +
                "}\n" +
                "public static long optLong(String key) {\n" +
                "\treturn optLong(key, 0l);\n" +
                "}\n" +
                "public static boolean optBoolean(String key) {\n" +
                "\treturn optBoolean(key, false);\n" +
                "}\n" +
                "public static double optDouble(String key) {\n" +
                "\treturn optDouble(key, 0.0d);\n" +
                "}");

        return sb.toString();
    }

    // 相关类型
    public static final String INTEGER = "int";
    public static final String FLOAT = "float";
    public static final String BOOLEAN = "boolean";
    public static final String STRING = "String";

    /**
     * @param value 给定的字符串
     * @return
     * @Name: confirmType
     * @Description: 根据给定的值, 确定给定的值的类型[估计]
     * @Create at 2016年6月25日 下午5:29:08 by '970655147'
     */
    private static String confirmType(String value) {
        String type = STRING;
        if (STRING.equals(type)) {
            try {
                Integer.parseInt(value);
                type = INTEGER;
            } catch (Exception e) {

            }
        }
        if (STRING.equals(type)) {
            if (Tools.equalsIgnoreCase(Constants.TRUE, value) || Tools.equalsIgnoreCase(Constants.FALSE, value)) {
                type = BOOLEAN;
            }
        }
        if (STRING.equals(type)) {
            try {
                Float.parseFloat(value);
                type = FLOAT;
            } catch (Exception e) {

            }
        }

        return type;
    }

    /**
     * @param line 给定的行
     * @return
     * @Name: isComment
     * @Description: 判断给定的行是否为注释行
     * @Create at 2016年6月25日 下午5:50:05 by '970655147'
     */
    private static boolean isEmptyOrComment(String line) {
        String trimmed = line.trim();
        if (Tools.isEmpty(trimmed)) {
            return true;
        }
        if (trimmed.startsWith("#") || trimmed.startsWith("//") || trimmed.startsWith("rem") || trimmed.startsWith(";")) {
            return true;
        }

        return false;
    }

    /**
     * @param key 给定的属性的名称
     * @return
     * @Name: getAttrName
     * @Description: 获取给定的属性的名称
     * @Create at 2016年6月25日 下午6:08:44 by '970655147'
     */
    private static String getAttrName(String key) {
        return "_" + Tools.camel2UnderLine(key).toUpperCase();
    }

    /**
     * @param key 给定的属性的名称
     * @return
     * @Name: getDefaultAttrName
     * @Description: 获取给定的属性的默认属性名
     * @Create at 2016年6月25日 下午6:05:49 by '970655147'
     */
    private static String getDefaultAttrName(String key) {
        return "DEFAULT_" + Tools.camel2UnderLine(key).toUpperCase();
    }

}
