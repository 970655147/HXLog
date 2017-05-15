package com.hx.log.validator;

import com.hx.common.interf.common.Code2Msg;
import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;

/**
 * ValidateResult
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 8:40 PM
 */
public class ValidateResult implements Result {

    /**
     * �Ƿ�ɹ�
     */
    private boolean success;
    /**
     * ��Ӧ��
     */
    private int code;
    /**
     * ��Ӧ��Ϣ
     */
    private String msg;
    /**
     * ��Ӧ����
     */
    private Object data;
    /**
     * ���������[��չ]
     */
    private Object extra;

    /**
     * ��ʼ��
     *
     * @param success �Ƿ�ɹ�
     * @param code    ��Ӧ��
     * @param msg     ��Ӧ��Ϣ
     * @param data    result������
     * @param extra   result���������
     * @since 1.0
     */
    public ValidateResult(boolean success, int code, String msg, Object data, Object extra) {
        this.success = success;
        this.code = code;
        this.msg = msg;
        this.data = data;
        this.extra = extra;
    }

    public ValidateResult(boolean success, int code, String msg, Object data) {
        this(success, code, msg, data, null);
    }

    public ValidateResult(boolean success, int code, String msg) {
        this(success, code, msg, null);
    }

    public ValidateResult(boolean success, Code2Msg<Integer, String> code2Msg, Object data, Object extra) {
        Tools.assert0(code2Msg != null, "'code2Msg' can't be null !");
        this.success = success;
        this.code = code2Msg.code();
        this.msg = code2Msg.msg();
        this.data = data;
        this.extra = extra;
    }

    public ValidateResult(boolean success, Code2Msg<Integer, String> code2Msg, Object data) {
        this(success, code2Msg, data, null);
    }

    public ValidateResult(boolean success, Code2Msg<Integer, String> code2Msg) {
        this(success, code2Msg, null);
    }

    @Override
    public boolean success() {
        return success;
    }

    @Override
    public int code() {
        return code;
    }

    @Override
    public String msg() {
        return msg;
    }

    @Override
    public Object data() {
        return data;
    }

    @Override
    public Object extra() {
        return extra;
    }
}
