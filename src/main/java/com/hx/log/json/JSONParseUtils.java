package com.hx.log.json;

import com.hx.log.json.interf.JSON;
import com.hx.log.json.interf.JSONType;
import com.hx.log.str.WordsSeprator;
import com.hx.log.util.Tools;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 * JSONParseUtils
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 5:49 PM
 */
final class JSONParseUtils {

    // disable constructor
    private JSONParseUtils() {
        Tools.assert0("can't instantiate !");
    }

    /**
     * �Ƴ�key�Աߵķָ���
     *
     * @param keyWithSep �Ա߰����˷ָ�����key
     * @param seps       keyWithSep�ķָ�����ѡ�б�
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 5:06 PM
     * @since 1.0
     */
    static String trimForSurroundSep(String keyWithSep, Collection<String> seps) {
        for (String sep : seps) {
            if (keyWithSep.startsWith(sep)) {
                return keyWithSep.substring(sep.length(), keyWithSep.length() - sep.length());
            }
        }
        Tools.assert0("key must startsWith : " + seps.toString());
        return keyWithSep;
    }

    /**
     * �ӵ�ǰSeprator����ȡ��һ��value, ������JSONStr, JSONInt, JSONBool, JSONObject, JSONArray
     *
     * @param sep seprator
     * @param key the key
     * @return com.hx.log.json.interf.JSON
     * @author Jerry.X.He
     * @date 4/15/2017 5:10 PM
     * @since 1.0
     */
    static JSON getNextValue(WordsSeprator sep, String key) {
        Tools.assert0(sep.hasNext(), "expect an value for key : " + key);
        String next = sep.seek().trim();
        if (JSONConstants.OBJ_START.equals(next)) {
            return JSONObject.fromString(sep, false);
        } else if (JSONConstants.ARR_START.equals(next)) {
            return JSONArray.fromString(sep, false);
        } else if (next.startsWith(JSONConstants.STR_SEP01) || next.startsWith(JSONConstants.STR_SEP02)) {
            sep.next();
            return JSONStr.fromObject(trimForSurroundSep(next, JSONConstants.KEY_SEPS));
        } else if (Tools.equalsIgnoreCase(Tools.TRUE, next) || Tools.equalsIgnoreCase(Tools.FALSE, next)) {
            sep.next();
            return JSONBool.fromObject(Tools.equalsIgnoreCase(Tools.TRUE, next));
        } else if (endsWith(next, JSONConstants.ELE_LONG_SUFFIXES)) {
            try {
                long longVal = Long.parseLong(next);
                sep.next();
                return JSONLong.fromObject(longVal);
            } catch (Exception e) {
                // ignore
            }
        } else if (endsWith(next, JSONConstants.ELE_FLOAT_SUFFIXES) ||
                // if text with '.', default choose it as float
                (next.contains(".")) && (!endsWith(next, JSONConstants.ELE_DOUBLE_SUFFIXES))) {
            try {
                float floatVal = Float.parseFloat(next);
                sep.next();
                return JSONFloat.fromObject(floatVal);
            } catch (Exception e) {
                // ignore
            }
        } else if (endsWith(next, JSONConstants.ELE_DOUBLE_SUFFIXES)) {
            try {
                double doubleVal = Double.parseDouble(next);
                sep.next();
                return JSONDouble.fromObject(doubleVal);
            } catch (Exception e) {
                // ignore
            }
        } else {
            try {
                int intVal = Integer.parseInt(next);
                sep.next();
                return JSONInt.fromObject(intVal);
            } catch (Exception e) {
                // ignore
            }
        }

        Tools.assert0("bad format value for key : " + key);
        return null;
    }


    /**
     * �жϸ������ַ����Ƿ� ƥ������ĺ�׺�б���ĳһ����׺
     *
     * @param str      �������ַ���
     * @param suffixes �����ı�ѡ��׺�б�
     * @return boolean
     * @author Jerry.X.He
     * @date 4/15/2017 6:17 PM
     * @since 1.0
     */
    static boolean endsWith(String str, Set<String> suffixes) {
        for (String suffix : suffixes) {
            if (str.endsWith(suffix)) {
                return true;
            }
        }

        return false;
    }

    /**
     * ��������JSONObject�����sb��[����ѹ��]
     *
     * @param obj          ������JSONObject
     * @param sb           ������ַ���
     * @return void
     * @author Jerry.X.He
     * @date 4/15/2017 10:39 PM
     * @since 1.0
     */
    static void toString(JSONObject obj, StringBuilder sb) {
        Tools.append(sb, JSONConstants.OBJ_START);

        for(Map.Entry<String, JSON> entry : obj.eles.entrySet()) {
            JSON value = entry.getValue();
            Tools.append(sb, "\"" + entry.getKey() + "\":");
            if(JSONType.OBJECT == value.type()) {
                Tools.append(sb, "");
                toString((JSONObject) value.value(), sb);
            } else if(JSONType.ARRAY == value.type()) {
                Tools.append(sb, "");
                toString((JSONArray) value.value(), sb);
            } else if((JSONType.OBJ == value.type()) || (JSONType.STR == value.type()) ){
                Tools.append(sb, "\"" + value.toString(0) + "\"" );
            } else {
                Tools.append(sb, value.toString(0) );
            }
            Tools.append(sb, ", ");
        }

        Tools.removeLastSep(sb, ", ");
        Tools.append(sb, JSONConstants.OBJ_END);
    }

    static void toString(JSONArray obj, StringBuilder sb) {
        Tools.append(sb, JSONConstants.ARR_START);

        for(JSON value : obj.eles) {
            if(JSONType.OBJECT == value.type()) {
                toString((JSONObject) value.value(), sb);
            } else if(JSONType.ARRAY == value.type()) {
                toString((JSONArray) value.value(), sb);
            } else if((JSONType.OBJ == value.type()) || (JSONType.STR == value.type()) ){
                Tools.append(sb, "\"" + value.toString(0) + "\"" );
            } else {
                Tools.append(sb, value.toString(0) );
            }
            Tools.append(sb, ", ");
        }

        Tools.removeLastSep(sb, ", ");
        Tools.append(sb, JSONConstants.ARR_END);
    }

    /**
     * ��������JSONObject��ʽ�������sb��
     *
     * @param obj          ������JSONObject
     * @param indentFactor ����
     * @param sb           ������ַ���
     * @return void
     * @author Jerry.X.He
     * @date 4/15/2017 10:39 PM
     * @since 1.0
     */
    static void toString(JSONObject obj, int indentFactor, int depth, StringBuilder sb) {
        int identCnt = indentFactor * depth;
        appendBackspace(sb, identCnt - indentFactor);
        Tools.appendCRLF(sb, JSONConstants.OBJ_START);

        for(Map.Entry<String, JSON> entry : obj.eles.entrySet()) {
            JSON value = entry.getValue();
            appendBackspace(sb, identCnt);
            Tools.append(sb, "\"" + entry.getKey() + "\" : ");
            if(JSONType.OBJECT == value.type()) {
                Tools.appendCRLF(sb, "");
                toString((JSONObject) value.value(), indentFactor, depth+1, sb);
            } else if(JSONType.ARRAY == value.type()) {
                Tools.appendCRLF(sb, "");
                toString((JSONArray) value.value(), indentFactor, depth+1, sb);
            } else if((JSONType.OBJ == value.type()) || (JSONType.STR == value.type()) ){
                Tools.append(sb, "\"" + value.toString(indentFactor) + "\"" );
            } else {
                Tools.append(sb, value.toString(indentFactor) );
            }
            Tools.appendCRLF(sb, ", ");
        }

        Tools.removeLastSep(sb, ", \r\n");
        Tools.appendCRLF(sb, "");
        appendBackspace(sb, identCnt - indentFactor);
        Tools.append(sb, JSONConstants.OBJ_END);
    }

    static void toString(JSONArray obj, int indentFactor, int depth, StringBuilder sb) {
        int identCnt = indentFactor * depth;
        appendBackspace(sb, identCnt - indentFactor);
        Tools.appendCRLF(sb, JSONConstants.ARR_START);

        for(JSON value : obj.eles) {
            if(JSONType.OBJECT == value.type()) {
                toString((JSONObject) value.value(), indentFactor, depth+1, sb);
            } else if(JSONType.ARRAY == value.type()) {
                toString((JSONArray) value.value(), indentFactor, depth+1, sb);
            } else if((JSONType.OBJ == value.type()) || (JSONType.STR == value.type()) ){
                appendBackspace(sb, identCnt);
                Tools.append(sb, "\"" + value.toString(indentFactor) + "\"" );
            } else {
                appendBackspace(sb, identCnt);
                Tools.append(sb, value.toString(indentFactor) );
            }
            Tools.appendCRLF(sb, ", ");
        }

        Tools.removeLastSep(sb, ", \r\n");
        Tools.appendCRLF(sb, "");
        appendBackspace(sb, identCnt - indentFactor);
        Tools.append(sb, JSONConstants.ARR_END);
    }

    /**
     * �������sb���identFactor���հ��ַ�
     *
     * @param sb          �������ַ���
     * @param identFactor ��Ҫ���Ŀհ��ַ��ĸ���
     * @return void
     * @author Jerry.X.He
     * @date 4/15/2017 10:41 PM
     * @since 1.0
     */
    private static void appendBackspace(StringBuilder sb, int identFactor) {
        for (int i = 0; i < identFactor; i++) {
            sb.append(" ");
        }
    }

}
