/**
 * file name : JSONUtils.java
 * created at : 22:38:27 2016-12-30
 * created by 970655147
 */

package com.hx.log.util;

import com.hx.log.util.interf.IdxIterator;
import java.util.Iterator;
import java.util.Set;

import net.sf.json.JSON;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

public final class JSONUtils {

    // disable constructor
    private JSONUtils() {
        Tools.assert0("can't instantiate !");
    }


    // add at 2016.06.21
    public static <K, V> boolean isJSONEmpty(JSONObject obj) {
        return (obj == null) || (obj.isNullObject()) || (obj.size() == 0) || (obj.isEmpty());
    }


    // ȥ����obj�����е��ַ������ֵ�����ڵĶ���ո�
    // ˼· : ���obj�ǿն���  ��ֱ�ӷ���
    // ���� ��������kv����, ���ֵΪString  ��ȥ�������Ŀո�, Ȼ���ڸ���obj�ж�Ӧkey��ֵ
    // ������� ֵΪJSONObject, �ݹ�
    // ������� ֵΪJSONArray, trimSpaces(JSONArray )
    public static void trimSpaces(JSONObject obj) {
        if (obj.isNullObject() || Tools.isEmpty(obj)) {
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

    // ȥ����arr�����е��ַ������ֵ�����ڵĶ���ո�
    // ˼· : ���arr�ǿն���  ��ֱ�ӷ���
    // ���� ������������, ���ֵΪString  ��ȥ�������Ŀո�, Ȼ���ڸ���obj�ж�Ӧkey��ֵ
    // ������� ֵΪJSONObject, trimSpaces(JSONObject )
    // ������� ֵΪJSONArray, �ݹ�
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

    // ȷ��arr�е�ÿһ��JSONObject������ָ����key, ����  ��ɾ������Ŀ
    // val.toString����ȷ��ֵΪnull������
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

    // ȥ����obj�����е��ַ������ֵ�����ڵĶ���ո�
    // ˼· : ���obj�ǿն���  ��ֱ�ӷ���
    // ���� ��������kv����, ���ֵΪString  ���ֻΪ��  ��ȥ����ǰkv��
    // ������� ֵΪJSONObject, �ݹ�,  ����÷���֮��, valΪ��, ���Ƴ�val��Ӧ����Ŀ
    // ������� ֵΪJSONArray, removeIfNull(JSONArray ),  ����÷���֮��, valΪ��, ���Ƴ�val��Ӧ����Ŀ
    public static void removeIfNull(JSONObject obj) {
        if (obj.isNullObject() || Tools.isEmpty(obj)) {
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

    // ȥ����arr�����е��ַ������ֵ�����ڵĶ���ո�
    // ˼· : ���arr�ǿն���  ��ֱ�ӷ���
    // ���� ������������, ���ֵΪString  ��ȥ�������Ŀո�, Ȼ���ڸ���obj�ж�Ӧkey��ֵ
    // ������� ֵΪJSONObject, removeIfNull(JSONObject ), ����÷���֮��, valΪ��, ���Ƴ�val��Ӧ����Ŀ
    // ע�� : ��Ϊ�����JSONArray������ɾ������, ��������ʹ����Iterator
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
     * @param obj            JSON����
     * @param needBeFiltered ��Ҫ���˵���key
     * @return
     * @Description: ��obj���Ƴ�needBeFiltered���������е�key
     * @Create at 2016-12-30 22:42:43 by '970655147'
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
     * @param pattern ������pattern
     * @param len     ��Ҫ��ȡ��JSONArray�ĳ���
     * @return
     * @Description: ���ݸ�����pattern����IdxIterator
     * @Create at 2016-12-31 14:04:43 by '970655147'
     */
    public static IdxIterator getIdxIteratorByPattern(String pattern, int len) {
        return JSONExtractor.getIdxIteratorByPattern(pattern, len);
    }

    /**
     * @param json    ������json����
     * @param pattern ��Ҫ��ȡ���ݵ�pattern
     * @return
     * @Description: ���ݸ�����pattern�Ӹ�����json����ȡ����
     * @Create at 2016-12-31 14:05:51 by '970655147'
     */
    public static JSONArray extractInfoFromJSON(JSON json, String pattern) {
        return JSONExtractor.extractInfoFromJSON(json, pattern);
    }


}
