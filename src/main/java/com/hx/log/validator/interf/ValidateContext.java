package com.hx.log.validator.interf;

/**
 * ValidateContext
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 10:12 PM
 */
public interface ValidateContext {

    /**
     * ��ҪУ���Ŀ�����
     *
     * @return the object that need to be validate
     * @author Jerry.X.He
     * @date 5/3/2017 10:13 PM
     * @since 1.0
     */
    Object obj();

    /**
     * У���validator
     *
     * @return the validator that occur in validate context
     * @author Jerry.X.He
     * @date 5/3/2017 10:13 PM
     * @since 1.0
     */
    Validator validator();

    /**
     * У���ʱ��Ķ������Ϣ
     *
     * @return the extra object that occur in validate context
     * @author Jerry.X.He
     * @date 5/3/2017 10:13 PM
     * @since 1.0
     */
    Object extra();

}
