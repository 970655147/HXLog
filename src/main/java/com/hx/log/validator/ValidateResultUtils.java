package com.hx.log.validator;

import com.hx.json.JSONObject;
import com.hx.log.interf.Code2Msg;
import com.hx.log.interf.Result;
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
     * ���ݸ����������װһ���ɹ��Ľ��
     *
     * @param code  ��Ӧ��
     * @param msg   ��Ӧ��Ϣ
     * @param data  ��Ӧ����
     * @param extra ��������
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
     * ���ݸ����������װһ��ʧ�ܵĽ��
     *
     * @param code  ��Ӧ��
     * @param msg   ��Ӧ��Ϣ
     * @param data  ��Ӧ����
     * @param extra ��������
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

    /**
     * ��������Resultת��Ϊһ��JSONObejct
     *
     * @param result
     * @return com.hx.json.JSONObject
     * @author Jerry.X.He
     * @date 5/4/2017 8:57 PM
     * @since 1.0
     */
    public static JSONObject toJSON(Result result) {
        return new JSONObject()
                .element("success", result.success()).element("code", result.code())
                .element("msg", result.msg()).element("data", result.data())
                .element("extra", result.extra())
                ;
    }

    /**
     * ���ݸ��������빹��һ��ValidateResult
     *
     * @param obj ������JSONObject
     * @return com.hx.log.interf.Result
     * @author Jerry.X.He
     * @date 5/4/2017 9:00 PM
     * @since 1.0
     */
    public static Result fromJSON(JSONObject obj) {
        boolean success = obj.optBoolean("success", false);
        int code = obj.optInt("code", ValidateErrorCode.FAILED.code());
        String msg = obj.optString("msg", ValidateErrorCode.FAILED.msg());
        Object data = obj.opt("data");
        Object extra = obj.opt("extra");
        return new ValidateResult(success, code, msg, data, extra);
    }

}
