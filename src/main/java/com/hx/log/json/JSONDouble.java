package com.hx.log.json;

import com.hx.log.json.interf.JSON;
import com.hx.log.json.interf.JSONType;

/**
 * JSONDouble
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 6:08 PM
 */
class JSONDouble implements JSON {

    /**
     * ��ǰJSON���е�val
     */
    private double val;

    JSONDouble(double val) {
        this.val = val;
    }

    @Override
    public JSONType type() {
        return JSONType.DOUBLE;
    }

    @Override
    public Object value() {
        return Double.valueOf(val);
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
     * ���ݸ�����ֵ����һ��JSONDouble
     *
     * @param val ������ֵ
     * @return com.hx.log.json.JSONStr
     * @author Jerry.X.He
     * @date 4/15/2017 5:18 PM
     * @since 1.0
     */
    static JSONDouble fromObject(double val) {
        return new JSONDouble(val);
    }

}
