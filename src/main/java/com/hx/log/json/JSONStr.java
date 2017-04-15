package com.hx.log.json;

import com.hx.log.json.interf.JSON;
import com.hx.log.json.interf.JSONType;
import com.hx.log.util.Tools;

/**
 * JSON�е��ַ���Ԫ��
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 11:55 AM
 */
class JSONStr implements JSON {

    /**
     * JSONStr���е��ַ���
     */
    private String str;

    JSONStr(String str) {
        this.str = str;
    }

    @Override
    public JSONType type() {
        return JSONType.STR;
    }

    @Override
    public Object value() {
        return str;
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
        return str;
    }

    /**
     * ���ݸ�����Object����һ��JSONStr
     *
     * @param obj ������Object
     * @return com.hx.log.json.JSONStr
     * @author Jerry.X.He
     * @date 4/15/2017 5:18 PM
     * @since 1.0
     */
    static JSONStr fromObject(Object obj) {
        return new JSONStr(String.valueOf(obj));
    }

}
