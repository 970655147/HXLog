package com.hx.log.validator.interf;

import com.hx.log.interf.Result;

import java.util.Iterator;

/**
 * 注册 obj -> validator 的工具
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 10:06 PM
 */
public interface ValidatorRegister {

    /**
     * 注册 给定的 obj -> validator
     *
     * @param obj       给定的需要校验的对象
     * @param validator 校验的工具
     * @param extra      附加参数
     * @return an ValidatorRegister after registered specified object -> validator
     * @author Jerry.X.He
     * @date 5/3/2017 10:07 PM
     * @since 1.0
     */
    ValidatorRegister register(Object obj, Validator validator, Object extra);

    /**
     * 注册 给定的 obj -> validator
     *
     * @param obj       给定的需要校验的对象
     * @param validator 校验的工具
     * @return an ValidatorRegister after registered specified object -> validator
     * @author Jerry.X.He
     * @date 5/3/2017 10:07 PM
     * @since 1.0
     */
    ValidatorRegister register(Object obj, Validator validator);

    /**
     * 获取所有的ValidateContext
     *
     * @return the iterator that could iterate all ValidateContext
     * @author Jerry.X.He
     * @date 5/3/2017 10:15 PM
     * @since 1.0
     */
    Iterator<ValidateContext> iterator();

    /**
     * 处理所有已经注册的 obj -> validator
     *
     * @return the result of all validator validated
     * @author Jerry.X.He
     * @date 5/3/2017 10:07 PM
     * @since 1.0
     */
    Result apply();

}
