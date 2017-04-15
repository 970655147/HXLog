package com.hx.log.json;

import com.hx.log.json.interf.JSON;
import com.hx.log.json.interf.JSONConfig;
import com.hx.log.json.interf.JSONType;
import com.hx.log.str.WordsSeprator;
import com.hx.log.util.Tools;

import java.util.*;

/**
 * JSONObject
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 11:50 AM
 */
public class JSONArray implements JSON, List<Object> {

    /**
     * һ����ʾ�յ�JSONArray��ʵ��
     */
    public static final JSONArray NULL_JSON_ARRAY = new JSONArray();

    /**
     * �洢��ǰArray������Ԫ��
     */
    List<JSON> eles;

    public JSONArray() {
        eles = new ArrayList<>();
    }

    /**
     * ��������Object����Ϊһ��JSONArray
     *
     * @param obj    ������Object
     * @param config ����JSONObject��ʱ�������
     * @return com.hx.log.json.JSONObject
     * @author Jerry.X.He
     * @date 4/15/2017 12:03 PM
     * @since 1.0
     */
    public static JSONArray fromObject(Object obj, JSONConfig config) {
        if (obj == null) {
            return NULL_JSON_ARRAY;
        }

        if (obj instanceof String) {
            return fromString((String) obj);
        }

        return null;
    }

    public static JSONArray fromObject(Object obj) {
        return fromObject(obj, new SimpleJSONConfig());
    }

    @Override
    public JSONType type() {
        return JSONType.ARRAY;
    }

    @Override
    public Object value() {
        return this;
    }

    @Override
    public boolean isEmpty() {
        return eles.isEmpty();
    }

    @Override
    public int size() {
        return eles.size();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(size() * String.valueOf(optString(0)).length());
        JSONParseUtils.toString(this, sb);
        return sb.toString();
    }

    @Override
    public String toString(int indentFactor) {
        StringBuilder sb = new StringBuilder(size() * String.valueOf(optString(0)).length());
        JSONParseUtils.toString(this, indentFactor, 1, sb);
        return sb.toString();
    }

    /**
     * �뵱ǰJSONArray�и����һ��Ԫ��
     *
     * @param val   ������value
     * @return com.hx.log.json.JSONObject
     * @author Jerry.X.He
     * @date 4/15/2017 6:29 PM
     * @since 1.0
     */
    public JSONArray element(Object val) {
        eles.add(JSONObj.fromObject(val));
        return this;
    }

    public JSONArray element(JSONObject val) {
        eles.add(val);
        return this;
    }

    public JSONArray element(JSONArray val) {
        eles.add(val);
        return this;
    }

    public JSONArray element(String val) {
        eles.add(JSONStr.fromObject(val));
        return this;
    }

    public JSONArray element(boolean val) {
        eles.add(JSONBool.fromObject(val));
        return this;
    }

    public JSONArray element(int val) {
        eles.add(JSONInt.fromObject(val));
        return this;
    }

    public JSONArray element(long val) {
        eles.add(JSONLong.fromObject(val));
        return this;
    }

    public JSONArray element(float val) {
        eles.add(JSONFloat.fromObject(val));
        return this;
    }

    public JSONArray element(double val) {
        eles.add(JSONDouble.fromObject(val));
        return this;
    }

    @Override
    public boolean add(Object o) {
        element(o);
        return true;
    }

    public void add(JSONObject val) {
        element(val);
    }

    public void add(JSONArray val) {
        element(val);
    }

    public void add(String val) {
        element(val);
    }

    public void add(boolean val) {
        element(val);
    }

    public void add(int val) {
        element(val);
    }

    public void add(long val) {
        element(val);
    }

    public void add(float val) {
        element(val);
    }

    public void add(double val) {
        element(val);
    }

    /**
     * ��ȡkey��Ӧ��Object
     * ���������, �������Ͳ�ƥ��, �׳��쳣
     *
     * @param idx ����������
     * @return java.lang.Object
     * @author Jerry.X.He
     * @date 4/15/2017 7:06 PM
     * @since 1.0
     */
    public Object get(int idx) {
        JSON val = eles.get(idx);
        if(val == null) {
            Tools.assert0("the element of [" + idx + "] do not exists !");
        }

        return val.value();
    }

    public JSONObject getJSONObject(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.OBJECT)) {
            Tools.assert0("the element of [" + idx + "] do not exists or it does not an JSONObject !");
        }

        return (JSONObject) val.value();
    }

    public JSONArray getJSONArray(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.OBJECT)) {
            Tools.assert0("the element of [" + idx + "] do not exists or it does not an JSONArray !");
        }

        return (JSONArray) val.value();
    }


    public String getString(int idx) {
        JSON val = eles.get(idx);
        if(val == null) {
            Tools.assert0("the element of [" + idx + "] do not exists !");
        }

        return String.valueOf(val.value());
    }

    public boolean getBoolean(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.BOOL)) {
            Tools.assert0("the element of [" + idx + "] do not exists or it does not an boolean !");
        }

        return (Boolean) val.value();
    }

    public int getInt(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.INT)) {
            Tools.assert0("the element of [" + idx + "] do not exists or it does not an int !");
        }

        return (Integer) val.value();
    }

    public long getLong(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.LONG)) {
            Tools.assert0("the element of [" + idx + "] do not exists or it does not an long !");
        }

        return (Long) val.value();
    }

    public float getFloat(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.FLOAT)) {
            Tools.assert0("the element of [" + idx + "] do not exists or it does not an float !");
        }

        return (Float) val.value();
    }

    public double getDouble(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.DOUBLE)) {
            Tools.assert0("the element of [" + idx + "] do not exists or it does not an double !");
        }

        return (Double) val.value();
    }

    /**
     * ��ȡkey��Ӧ��Object
     * ���������, �������Ͳ�ƥ��, ����Ĭ�Ͻ��
     *
     * @param idx ����������
     * @return java.lang.Object
     * @author Jerry.X.He
     * @date 4/15/2017 7:06 PM
     * @since 1.0
     */
    public Object opt(int idx) {
        JSON val = eles.get(idx);
        if(val == null) {
            return null;
        }

        return val.value();
    }

    public JSONObject optJSONObject(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.OBJECT)) {
            return null;
        }

        return (JSONObject) val.value();
    }

    public JSONArray optJSONArray(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.OBJECT)) {
            return null;
        }

        return (JSONArray) val.value();
    }

    public String optString(int idx) {
        JSON val = eles.get(idx);
        if(val == null) {
            return null;
        }

        return String.valueOf(val.value());
    }

    public boolean optBoolean(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.BOOL)) {
            return false;
        }

        return (Boolean) val.value();
    }

    public int optInt(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.INT)) {
            return 0;
        }

        return (Integer) val.value();
    }

    public long optLong(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.LONG)) {
            return 0L;
        }

        return (Long) val.value();
    }

    public float optFloat(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.FLOAT)) {
            return 0F;
        }

        return (Float) val.value();
    }

    public double optDouble(int idx) {
        JSON val = eles.get(idx);
        if(val == null || (val.type() != JSONType.DOUBLE)) {
            return 0D;
        }

        return (Double) val.value();
    }

    @Override
    public boolean contains(Object o) {
        for(Object ele : this) {
            if(Objects.equals(ele, o) ) {
                return true;
            }
        }

        return false;
    }

    @Override
    public Iterator<Object> iterator() {
        return new InternalIterator(eles.iterator());
    }

    @Override
    public Object[] toArray() {
        Object[] res = new Object[size()];
        int idx = 0;
        for(Object ele : this) {
            res[idx ++] = ele;
        }
        return res;
    }

    @Override
    public <T> T[] toArray(T[] a) {
        throw new RuntimeException("Unsupported Operation Exception !");
    }

    @Override
    public boolean remove(Object o) {
        Iterator<Object> ite = iterator();
        while(ite.hasNext()) {
            if(Objects.equals(ite.next(), o) ) {
                ite.remove();
            }
        }

        return true;
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        throw new RuntimeException("Unsupported Operation Exception !");
    }

    @Override
    public boolean addAll(Collection<?> c) {
        for(Object ele : c) {
            add(ele);
        }
        return true;
    }

    @Override
    public boolean addAll(int index, Collection<?> c) {
        List<JSON> collected = new ArrayList<>(c.size());
        for(Object ele : c) {
            collected.add(JSONObj.fromObject(ele));
        }
        return eles.addAll(index, collected);
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        for(Object ele : c) {
            remove(ele);
        }
        return true;
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        boolean modified = false;
        Iterator<Object> ite = iterator();
        while(ite.hasNext()) {
            if(!c.contains(ite.next())) {
                ite.remove();
                modified = true;
            }
        }
        return modified;
    }

    @Override
    public Object set(int index, Object element) {
        return eles.set(index, JSONObj.fromObject(element));
    }

    @Override
    public void add(int index, Object element) {
        eles.add(index, JSONObj.fromObject(element));
    }

    @Override
    public int indexOf(Object o) {
        int idx = 0;
        for(Object ele : this) {
            if(Objects.equals(ele, o)) {
                return idx;
            }
            idx ++;
        }

        return -1;
    }

    @Override
    public int lastIndexOf(Object o) {
        return 0;
    }

    @Override
    public ListIterator<Object> listIterator() {
        return new InternalListIterator(eles.listIterator());
    }

    @Override
    public ListIterator<Object> listIterator(int index) {
        return new InternalListIterator(eles.listIterator(index));
    }

    @Override
    public List<Object> subList(int fromIndex, int toIndex) {
        throw new RuntimeException("Unsupported Operation Exception !");
    }

    /**
     * �Ƴ���ǰJSONObject��key��Ӧ����Ŀ
     *
     * @param idx ����������
     * @return com.hx.log.json.interf.JSON
     * @author Jerry.X.He
     * @date 4/15/2017 6:44 PM
     * @since 1.0
     */
    public Object remove(int idx) {
        return eles.remove(idx).value();
    }

    /**
     * �Ƴ���ǰJSONObject�����е���Ŀ
     *
     * @return com.hx.log.json.interf.JSON
     * @author Jerry.X.He
     * @date 4/15/2017 6:44 PM
     * @since 1.0
     */
    public void clear() {
        eles.clear();
    }

    // ----------------- �������ݽṹ -----------------------
    /**
     * Iterator
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 4/15/2017 10:30 PM
     */
    private static class InternalIterator implements Iterator<Object> {
        Iterator<JSON> ite;
        public InternalIterator(Iterator<JSON> ite) {
            this.ite = ite;
        }
        @Override
        public boolean hasNext() {
            return ite.hasNext();
        }
        @Override
        public Object next() {
            JSON next = ite.next();
            return (next != null) ? next.value() : null;
        }
        @Override
        public void remove() {
            ite.remove();
        }
    }

    /**
     * ListIterator
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 4/15/2017 10:30 PM
     */
    private static class InternalListIterator implements ListIterator<Object> {
        ListIterator<JSON> ite;
        public InternalListIterator(ListIterator<JSON> ite) {
            this.ite = ite;
        }
        @Override
        public boolean hasNext() {
            return ite.hasNext();
        }
        @Override
        public Object next() {
            JSON res = ite.next();
            return (res != null) ? res.value() : null;
        }
        @Override
        public boolean hasPrevious() {
            return ite.hasPrevious();
        }
        @Override
        public Object previous() {
            JSON res = ite.previous();
            return (res != null) ? res.value() : null;
        }
        @Override
        public int nextIndex() {
            return ite.nextIndex();
        }
        @Override
        public int previousIndex() {
            return ite.previousIndex();
        }
        @Override
        public void remove() {
            ite.remove();
        }
        @Override
        public void set(Object o) {
            ite.set(JSONObj.fromObject(o));
        }
        @Override
        public void add(Object o) {
            ite.add(JSONObj.fromObject(o));
        }
    }
    // ----------------- �������� -----------------------

    /**
     * ����������seprator��ʣ��Ĳ���, �������Ϊһ��JSONObject
     *
     * @param sep ������seprator
     * @return com.hx.log.json.JSONObject
     * @author Jerry.X.He
     * @date 4/15/2017 5:32 PM
     * @since 1.0
     */
    static JSONArray fromString(WordsSeprator sep, boolean checkEnd) {
        Tools.assert0(JSONConstants.ARR_START.equals(sep.next()), "expect a : " + JSONConstants.ARR_START + " ! around : " + sep.currentAndRest());
        JSONArray result = new JSONArray();

        int idx = 0;
        while (sep.hasNext()) {
            JSON nextValue = JSONParseUtils.getNextValue(sep, "[" + idx + "]");
            result.eles.add(nextValue);

            if (JSONConstants.ARR_END.equals(sep.seek())) {
                break;
            }
            Tools.assert0(JSONConstants.ELE_SEP.equals(sep.next()), "expect a : " + JSONConstants.ELE_SEP + " ! around : " + sep.currentAndRest());
            idx ++;
        }
        // skip ']'
        sep.next();
        if(checkEnd) {
            Tools.assert0(Tools.isEmpty(sep.next()), "expect nothing after ']' !");
        }
        return result;
    }

    static JSONArray fromString(String str) {
        WordsSeprator sep = new WordsSeprator(str, JSONConstants.JSON_SEPS, JSONConstants.NEED_TO_ESCAPE, true, false);
        return fromString(sep, true);
    }

}
