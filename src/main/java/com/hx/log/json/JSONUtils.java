/**
 * file name : JSONUtils.java
 * created at : 22:38:27 2016-12-30
 * created by 970655147
 */

package com.hx.log.json;

import com.hx.json.JSONArray;
import com.hx.json.JSONObject;
import com.hx.json.interf.JSON;
import com.hx.common.interf.idx.IdxIterator;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.hx.log.json.interf.CheckRequiredAttributeFunc;
import com.hx.log.util.Tools;

import static com.hx.log.util.Tools.assert0;

/**
 * JSON��صĹ�����
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 4:26 PM
 */
public final class JSONUtils {

    // disable constructor
    private JSONUtils() {
        assert0("can't instantiate !");
    }


    // add at 2016.06.21

    /**
     * �жϸ�����JSONObject�Ƿ�Ϊ��
     *
     * @param obj ������JSONObject
     * @return boolean
     * @author Jerry.X.He
     * @date 5/5/2017 4:18 PM
     * @since 1.0
     */
    public static <K, V> boolean isJSONEmpty(JSONObject obj) {
        return (obj == null) || (obj.isNull()) || (obj.size() == 0) || (obj.isEmpty());
    }

    /**
     * ȥ����obj�����е��ַ������ֵ�����ڵĶ���ո�
     * ˼· : ���obj�ǿն���  ��ֱ�ӷ���
     * ���� ��������kv����, ���ֵΪString  ��ȥ�������Ŀո�, Ȼ���ڸ���obj�ж�Ӧkey��ֵ
     * ������� ֵΪJSONObject, �ݹ�
     * ������� ֵΪJSONArray, trimSpaces(JSONArray )
     *
     * @param obj ������JSONObject
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:18 PM
     * @since 1.0
     */
    public static void trimSpaces(JSONObject obj) {
        if (obj.isNull() || Tools.isEmpty(obj)) {
            return;
        }

        JSONArray names = obj.names();
        Iterator<?> it = names.iterator();
        while (it.hasNext()) {
            String key = String.valueOf(it.next());
            Object val = obj.get(key);
            if (val instanceof String) {
                obj.put(key, Tools.trimSpacesAsOne((String) val));
            } else if (val instanceof JSONObject) {
                trimSpaces((JSONObject) val);
            } else if (val instanceof JSONArray) {
                trimSpaces((JSONArray) val);
            }
        }
    }

    /**
     * ȥ����arr�����е��ַ������ֵ�����ڵĶ���ո�
     * ˼· : ���arr�ǿն���  ��ֱ�ӷ���
     * ���� ������������, ���ֵΪString  ��ȥ�������Ŀո�, Ȼ���ڸ���obj�ж�Ӧkey��ֵ
     * ������� ֵΪJSONObject, trimSpaces(JSONObject )
     * ������� ֵΪJSONArray, �ݹ�
     *
     * @param arr ������JSONArray
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:19 PM
     * @since 1.0
     */
    public static void trimSpaces(JSONArray arr) {
        if (Tools.isEmpty(arr)) {
            return;
        }

        for (int i = 0; i < arr.size(); i++) {
            Object val = arr.get(i);
            if (val instanceof String) {
                arr.set(i, Tools.trimSpacesAsOne((String) val));
            } else if (val instanceof JSONObject) {
                trimSpaces((JSONObject) val);
            } else if (val instanceof JSONArray) {
                trimSpaces((JSONArray) val);
            }
        }
    }

    /**
     * ȷ��arr�е�ÿһ��JSONObject������ָ����key, ����  ��ɾ������Ŀ
     * val.toString����ȷ��ֵΪnull������
     *
     * @param arr ������JSONArray
     * @param key ��Ҫȷ����key
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:20 PM
     * @since 1.0
     */
    public static void removeIfNull(JSONArray arr, String key) {
        if (Tools.isEmpty(arr)) {
            return;
        }

        Iterator<?> it = arr.iterator();
        while (it.hasNext()) {
            Object obj = it.next();
            if (obj instanceof JSONObject) {
                if (!((JSONObject) obj).containsKey(key)) {
                    it.remove();
                } else {
                    Object val = ((JSONObject) obj).get(key);
                    // 1. "{'key' : '' }"
                    // 2. "{'key' : null }"
                    if (Tools.isEmpty(val.toString())) {
                        it.remove();
                    }
                }
            }

        }
    }

    /**
     * ȥ����obj�����е�null����
     * ˼· : ���obj�ǿն���  ��ֱ�ӷ���
     * ���� ��������kv����, ���ֵΪString  ���ֻΪ��  ��ȥ����ǰkv��
     * ������� ֵΪJSONObject, �ݹ�,  ����÷���֮��, valΪ��, ���Ƴ�val��Ӧ����Ŀ
     * ������� ֵΪJSONArray, removeIfNull(JSONArray ),  ����÷���֮��, valΪ��, ���Ƴ�val��Ӧ����Ŀ
     *
     * @param obj ������JSONObject
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:21 PM
     * @since 1.0
     */
    public static void removeIfNull(JSONObject obj) {
        if (obj.isNull() || Tools.isEmpty(obj)) {
            return;
        }

        Iterator<?> it = obj.names().iterator();
        while (it.hasNext()) {
            String key = String.valueOf(it.next());
            Object val = obj.get(key);
            if (val instanceof String) {
                if (Tools.isEmpty((String) val)) {
                    obj.remove(key);
                }
            } else if (val instanceof JSONObject) {
                // ��ֹ "{'price' : null } "������
                if (Tools.isEmpty(val.toString())) {
                    obj.remove(key);
                } else {
                    removeIfNull((JSONObject) val);
                    if (((JSONObject) val).isEmpty()) {
                        obj.remove(key);
                    }
                }
            } else if (val instanceof JSONArray) {
                removeIfNull((JSONArray) val);
                if (((JSONArray) val).isEmpty()) {
                    obj.remove(key);
                }
            }
        }
    }

    /**
     * ȥ����arr�����е�null����
     * ˼· : ���arr�ǿն���  ��ֱ�ӷ���
     * ���� ������������, ���ֵΪString  ��ȥ�������Ŀո�, Ȼ���ڸ���obj�ж�Ӧkey��ֵ
     * ������� ֵΪJSONObject, removeIfNull(JSONObject ), ����÷���֮��, valΪ��, ���Ƴ�val��Ӧ����Ŀ
     * ע�� : ��Ϊ�����JSONArray������ɾ������, ��������ʹ����Iterator
     *
     * @param arr ������JSONArray
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:21 PM
     * @since 1.0
     */
    public static void removeIfNull(JSONArray arr) {
        if (Tools.isEmpty(arr)) {
            return;
        }

        Iterator<?> it = arr.iterator();
        while (it.hasNext()) {
            Object val = it.next();
            if (val instanceof String) {
                if (Tools.isEmpty((String) val)) {
                    it.remove();
                }
            } else if (val instanceof JSONObject) {
                // ��ֹ "{'price' : null } "������
                if (Tools.isEmpty(val.toString())) {
                    it.remove();
                } else {
                    removeIfNull((JSONObject) val);
                    if (((JSONObject) val).isEmpty()) {
                        it.remove();
                    }
                }
            }
        }
    }

    /**
     * ��obj���Ƴ�needBeFiltered���������е�key
     *
     * @param obj            JSON����
     * @param needBeFiltered ��Ҫ���˵���key�ļ���
     * @return com.hx.json.JSONObject
     * @author Jerry.X.He
     * @date 5/5/2017 4:22 PM
     * @since 1.0
     */
    public static JSONObject filter(JSONObject obj, Set<String> needBeFiltered) {
        if (Tools.isEmpty(obj) || Tools.isEmpty(needBeFiltered)) {
            return obj;
        }

        for (String filter : needBeFiltered) {
            obj.remove(filter);
        }
        return obj;
    }

    /**
     * ���ݸ�����pattern����IdxIterator
     *
     * @param pattern ������pattern
     * @param len     ��Ҫ��ȡ��JSONArray�ĳ���
     * @return com.hx.common.interf.idx.IdxIterator
     * @author Jerry.X.He
     * @date 5/5/2017 4:23 PM
     * @since 1.0
     */
    public static IdxIterator getIdxIteratorByPattern(String pattern, int len) {
        return JSONExtractor.getIdxIteratorByPattern(pattern, len);
    }

    /**
     * ���ݸ�����pattern�Ӹ�����json����ȡ����
     *
     * @param json    ������json����
     * @param pattern ��Ҫ��ȡ���ݵ�pattern
     * @return com.hx.json.JSONArray
     * @author Jerry.X.He
     * @date 5/5/2017 4:23 PM
     * @since 1.0
     */
    public static JSONArray extractInfoFromJSON(JSON json, String pattern) {
        return JSONExtractor.extractInfoFromJSON(json, pattern);
    }

    /**
     * ȷ��obj�и����ı������Դ���, ���ҺϷ�
     *
     * @param obj           ������JSONObject
     * @param requiredAttrs ����Ҫ���ڵ�����
     * @param checkFunc     У��������Եĺ���
     * @return boolean return true if all of 'requiredAttrs' in 'obj' does exists and valid, or else
     * @author 970655147 created at 2017-03-19 18:43
     */
    public static boolean checkJSONObj(JSONObject obj, Collection<String> requiredAttrs, CheckRequiredAttributeFunc checkFunc) {
        assert0(requiredAttrs != null, "'requiredAttrs' can't be null !");
        if (Tools.isEmpty(obj)) {
            return false;
        }

        for (String required : requiredAttrs) {
            if (!obj.containsKey(required)) {
                return false;
            }
            if (checkFunc != null) {
                boolean valid = checkFunc.check(obj, required);
                if (!valid) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * ȷ��arr��ÿһ��Ԫ�صĸ����ı������Դ���, ���ҺϷ�
     *
     * @param arr           ������JSONArray
     * @param requiredAttrs ����Ҫ���ڵ�����
     * @param checkFunc     У��������Եĺ���
     * @return boolean return true if all of 'requiredAttrs' in 'obj' does exists and valid, or else
     * @author 970655147 created at 2017-03-19 18:43
     */
    public static boolean checkJSONArr(JSONArray arr, Collection<String> requiredAttrs, CheckRequiredAttributeFunc checkFunc) {
        assert0(requiredAttrs != null, "'requiredAttrs' can't be null !");
        if (Tools.isEmpty(arr)) {
            return false;
        }

        for (int i = 0, len = arr.size(); i < len; i++) {
            JSONObject obj = arr.getJSONObject(i);
            if (obj == null) {
                return false;
            }
            boolean valid = checkJSONObj(obj, requiredAttrs, checkFunc);
            if (!valid) {
                return false;
            }
        }

        return true;
    }

    /**
     * ��spec�л�ȡ��Ҫ������
     * ע�� : ����ȷ��spec��ÿһ������ΪJSONObject, nameΪspec����������Ҫ����ֵ, valueΪspec����������Ҫ��ȡ��ֵ,
     * getInSpec��Ż�ȡ���ݵļ���(key[srcԴ����] -> key[dstĿ�����])ӳ��
     * <p>
     * demo 01 .
     * <p>
     * arr : [{"name":"hx","age":21},{"name":"zhangsan","age":33}]
     * result : { }
     * nameKeyInArr : 'name', valueKeyInArr : 'age'
     * key2NewKey : {"hx":"hxAlias","zhangsan'sName":"zhangsan'sName"}
     * ----------------------------extractValueAsKv-------------------------
     * result : {"hxAlias":21, "zhangsan":33}
     * </p>
     * <p>
     * demo 02 .
     * [
     * ...												{
     * {												...
     * "value":" #F3F07AAR#ABA",		=>	    		"model":"#F3F07AAR#ABA"
     * "name":"Model"								    ...
     * }                                                }
     * ...
     * ]
     *
     * @param arr           ����������, ÿһ��Ԫ��ΪJSONObject
     * @param result        �������ݵ�JSONObject
     * @param nameKeyInArr  ��spec������������ȡname��Ϊkey[getInSpec]
     * @param valueKeyInArr ��spec������������ȡvalue��Ϊvalue
     * @param key2NewKey    ��Ҫ��ȡ�� key -> newKey��ӳ��
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 10:34 PM
     * @since 1.0
     */
    public static void extractValueAsKv(JSONArray arr, JSONObject result, String nameKeyInArr, String valueKeyInArr,
                                        Map<String, String> key2NewKey) {
        if ((Tools.isEmpty(arr)) || (Tools.isEmpty(result)) || Tools.isEmpty(key2NewKey)) {
            return;
        }

        Iterator<?> it = arr.iterator();
        while (it.hasNext()) {
            JSONObject val = (JSONObject) it.next();
            if (val != null) {
                String key = val.getString(nameKeyInArr);
                if (key2NewKey.containsKey(key)) {
                    result.put(key2NewKey.get(key), val.get(valueKeyInArr));
                }
            }
        }
    }

    public static void extractValueAsKv(JSONArray arr, JSONObject result, String nameKeyInArr, String valueKeyInArr) {
        if ((Tools.isEmpty(arr)) || (Tools.isEmpty(result))) {
            return;
        }

        Iterator<?> it = arr.iterator();
        while (it.hasNext()) {
            JSONObject val = (JSONObject) it.next();
            if (val != null) {
                String key = val.getString(nameKeyInArr);
                result.put(key, val.get(valueKeyInArr));
            }
        }
    }

}
