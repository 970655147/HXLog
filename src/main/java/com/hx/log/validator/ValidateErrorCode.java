package com.hx.log.validator;

import com.hx.log.idx.interf.IdxIterator;
import com.hx.log.interf.Code2Msg;

/**
 * У�����Ĵ�����
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 8:52 PM
 */
public enum ValidateErrorCode implements Code2Msg<Integer, String> {

    /**
     * ������ϸ�ʽ
     */
    SUCCESS(nextCode(), "success"),
    /**
     * ��Ӧʧ��
     */
    FAILED(nextCode(), "failed"),
    /**
     * ���ش���
     */
    FATAL(nextCode(), "fatal"),
    /**
     * ĳЩӦ��������ϸ�ʽ�ĳ���, ���������벻�Ϸ������
     */
    INPUT_NOT_FORMAT(nextCode(), "input not format !"),
    /**
     * ���������벻�ڷ�Χ��
     */
    NOT_IN_RANGE(nextCode(), "not in range !"),;

    /**
     * ��Ӧ��
     */
    private Integer code;
    /**
     * ��Ӧ���Ӧ����Ϣ
     */
    private String msg;

    /**
     * ��ʼ��
     *
     * @param code ��Ӧ��
     * @param msg  ��Ϣ
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
     * ��ȡ��һ��code
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
