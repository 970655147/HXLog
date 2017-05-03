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
     * 需要校验的目标对象
     *
     * @return the object that need to be validate
     * @author Jerry.X.He
     * @date 5/3/2017 10:13 PM
     * @since 1.0
     */
    Object obj();

    /**
     * 校验的validator
     *
     * @return the validator that occur in validate context
     * @author Jerry.X.He
     * @date 5/3/2017 10:13 PM
     * @since 1.0
     */
    Validator validator();

    /**
     * 校验的时候的额外的信息
     *
     * @return the extra object that occur in validate context
     * @author Jerry.X.He
     * @date 5/3/2017 10:13 PM
     * @since 1.0
     */
    Object extra();

}
