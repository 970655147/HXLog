package com.hx.log.validator;

import com.hx.log.interf.Code2Msg;
import com.hx.log.util.Tools;

/**
 * ValidateResultUtils
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 8:57 PM
 */
public final class ValidateResultUtils {

    // disable constructor
    private ValidateResultUtils() {
        Tools.assert0("can't instantiate !");
    }


    /**
     * 根据给定的输入封装一个成功的结果
     *
     * @param code  响应码
     * @param msg   响应消息
     * @param data  响应数据
     * @param extra 额外数据
     * @return com.hx.log.validator.ValidateResult
     * @author Jerry.X.He
     * @date 5/3/2017 9:00 PM
     * @since 1.0
     */
    public static ValidateResult success(int code, String msg, Object data, Object extra) {
        return new ValidateResult(true, code, msg, data, extra);
    }

    public static ValidateResult success(Code2Msg<Integer, String> code2Msg, Object data, Object extra) {
        return success(code2Msg.code(), code2Msg.msg(), data, extra);
    }

    public static ValidateResult success(int code, String msg, Object data) {
        return success(code, msg, data, null);
    }

    public static ValidateResult success(Code2Msg<Integer, String> code2Msg, Object data) {
        return success(code2Msg.code(), code2Msg.msg(), data);
    }

    public static ValidateResult success(Object data, Object extra) {
        return success(ValidateErrorCode.SUCCESS, data, extra);
    }

    public static ValidateResult success(Object data) {
        return success(data, null);
    }

    public static ValidateResult success() {
        return success(null);
    }

    /**
     * 根据给定的输入封装一个失败的结果
     *
     * @param code  响应码
     * @param msg   响应消息
     * @param data  响应数据
     * @param extra 额外数据
     * @return com.hx.log.validator.ValidateResult
     * @author Jerry.X.He
     * @date 5/3/2017 9:00 PM
     * @since 1.0
     */
    public static ValidateResult failed(int code, String msg, Object data, Object extra) {
        return new ValidateResult(false, code, msg, data, extra);
    }

    public static ValidateResult failed(Code2Msg<Integer, String> code2Msg, Object data, Object extra) {
        return failed(code2Msg.code(), code2Msg.msg(), data, extra);
    }

    public static ValidateResult failed(int code, String msg, Object data) {
        return failed(code, msg, data, null);
    }

    public static ValidateResult failed(Code2Msg<Integer, String> code2Msg, Object data) {
        return failed(code2Msg.code(), code2Msg.msg(), data);
    }

    public static ValidateResult failed(Object data, Object extra) {
        return failed(ValidateErrorCode.FAILED, data, extra);
    }

    public static ValidateResult failed(Object data) {
        return failed(data, null);
    }

    public static ValidateResult failed() {
        return failed(null);
    }

}
