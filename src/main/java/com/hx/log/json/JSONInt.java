package com.hx.log.json;

import com.hx.log.json.interf.JSON;
import com.hx.log.json.interf.JSONType;

/**
 * JSONInt
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 11:58 AM
 */
class JSONInt implements JSON {

    /**
     * ��ǰJSON���е�val
     */
    private int val;

    JSONInt(int val) {
        this.val = val;
    }

    @Override
    public JSONType type() {
        return JSONType.INT;
    }

    @Override
    public Object value() {
        return Integer.valueOf(val);
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
     * ���ݸ�����ֵ����һ��JSONInt
     *
     * @param val ������ֵ
     * @return com.hx.log.json.JSONStr
     * @author Jerry.X.He
     * @date 4/15/2017 5:18 PM
     * @since 1.0
     */
    static JSONInt fromObject(int val) {
        return new JSONInt(val);
    }

}
