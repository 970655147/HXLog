package com.hx.log.json;

import com.hx.log.json.interf.JSON;
import com.hx.log.json.interf.JSONType;

/**
 * JSONBool
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 11:59 AM
 */
class JSONBool implements JSON {

    /**
     * ��ǰJSON���е�val
     */
    private boolean val;

    JSONBool(boolean val) {
        this.val = val;
    }

    @Override
    public JSONType type() {
        return JSONType.BOOL;
    }

    @Override
    public Object value() {
        return Boolean.valueOf(val);
    }

    @Override
    public boolean isEmpty() {
        return true;
    }

    @Override
    public boolean isNull() {
        return false;
    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    public String toString(int indentFactor) {
        return String.valueOf(val);
    }

    /**
     * ���ݸ�����ֵ����һ��JSONBool
     *
     * @param bool ������ֵ
     * @return com.hx.log.json.JSONStr
     * @author Jerry.X.He
     * @date 4/15/2017 5:18 PM
     * @since 1.0
     */
    static JSONBool fromObject(boolean bool) {
        return new JSONBool(bool);
    }

}
