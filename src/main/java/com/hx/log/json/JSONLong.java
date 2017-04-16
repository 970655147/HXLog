package com.hx.log.json;

import com.hx.log.json.interf.JSON;
import com.hx.log.json.interf.JSONType;

/**
 * JSONLong
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 6:06 PM
 */
class JSONLong implements JSON {

    /**
     * ��ǰJSON���е�val
     */
    private long val;

    JSONLong(long val) {
        this.val = val;
    }

    @Override
    public JSONType type() {
        return JSONType.LONG;
    }

    @Override
    public Object value() {
        return Long.valueOf(val);
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
     * ���ݸ�����ֵ����һ��JSONLong
     *
     * @param val ������ֵ
     * @return com.hx.log.json.JSONStr
     * @author Jerry.X.He
     * @date 4/15/2017 5:18 PM
     * @since 1.0
     */
    static JSONLong fromObject(long val) {
        return new JSONLong(val);
    }

}
