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
     * 是否成功
     */
    private boolean success;
    /**
     * 响应码
     */
    private int code;
    /**
     * 响应消息
     */
    private String msg;
    /**
     * 响应数据
     */
    private Object data;
    /**
     * 额外的数据[扩展]
     */
    private Object extra;

    /**
     * 初始化
     *
     * @param success 是否成功
     * @param code    响应码
     * @param msg     响应消息
     * @param data    result的数据
     * @param extra   result额外的数据
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
