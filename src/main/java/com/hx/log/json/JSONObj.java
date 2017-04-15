package com.hx.log.json;

import com.hx.log.json.interf.JSON;
import com.hx.log.json.interf.JSONType;

/**
 * ����һ��Object��JSON
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 5:21 PM
 */
public class JSONObj implements JSON {

    /**
     * ��ǰJSON���е�Object
     */
    private Object obj;

    JSONObj(Object obj) {
        this.obj = obj;
    }

    @Override
    public JSONType type() {
        return JSONType.OBJ;
    }

    @Override
    public Object value() {
        return obj;
    }

    @Override
    public boolean isEmpty() {
        return true;
    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    public String toString(int indentFactor) {
        return String.valueOf(obj);
    }

    /**
     * ���ݸ�����Object����һ��JSONObj
     *
     * @param obj ������Object
     * @return com.hx.log.json.JSONStr
     * @author Jerry.X.He
     * @date 4/15/2017 5:18 PM
     * @since 1.0
     */
    static JSONObj fromObject(Object obj) {
        return new JSONObj(obj);
    }

}
