package com.hx.log.validator;

import com.hx.log.idx.interf.IdxIterator;
import com.hx.log.interf.Code2Msg;

/**
 * 校验结果的错误码
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 8:52 PM
 */
public enum ValidateErrorCode implements Code2Msg<Integer, String> {

    /**
     * 输出符合格式
     */
    SUCCESS(nextCode(), "success"),
    /**
     * 响应失败
     */
    FAILED(nextCode(), "failed"),
    /**
     * 严重错误
     */
    FATAL(nextCode(), "fatal"),
    /**
     * 某些应该输入符合格式的场景, 出现了输入不合法的情况
     */
    INPUT_NOT_FORMAT(nextCode(), "input not format !"),
    /**
     * 给定的输入不在范围内
     */
    NOT_IN_RANGE(nextCode(), "not in range !"),;

    /**
     * 响应码
     */
    private Integer code;
    /**
     * 响应码对应的消息
     */
    private String msg;

    /**
     * 初始化
     *
     * @param code 响应码
     * @param msg  消息
     * @return
     * @author
     * @date
     * @since 1.0
     */
    ValidateErrorCode(Integer code, String msg) {
        this.code = code;
        this.msg = msg;
    }

    @Override
    public Integer code() {
        return code;
    }

    @Override
    public String msg() {
        return msg;
    }

    /**
     * 获取下一个code
     *
     * @return java.lang.Integer
     * @author Jerry.X.He
     * @date 5/3/2017 8:56 PM
     * @since 1.0
     */
    private static Integer nextCode() {
        return IdxGenerator.next();
    }

    /**
     * IdxGenerator
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 5/3/2017 10:41 PM
     */
    private static class IdxGenerator {
        private static IdxIterator idxIterator = new com.hx.log.idx.IdxGenerator();
        static int next() {
            return idxIterator.next();
        }
    }
}
