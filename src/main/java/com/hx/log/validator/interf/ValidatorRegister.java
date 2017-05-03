package com.hx.log.validator.interf;

import com.hx.log.interf.Result;

import java.util.Iterator;

/**
 * ע�� obj -> validator �Ĺ���
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 10:06 PM
 */
public interface ValidatorRegister {

    /**
     * ע�� ������ obj -> validator
     *
     * @param obj       ��������ҪУ��Ķ���
     * @param validator У��Ĺ���
     * @param extra      ���Ӳ���
     * @return an ValidatorRegister after registered specified object -> validator
     * @author Jerry.X.He
     * @date 5/3/2017 10:07 PM
     * @since 1.0
     */
    ValidatorRegister register(Object obj, Validator validator, Object extra);

    /**
     * ע�� ������ obj -> validator
     *
     * @param obj       ��������ҪУ��Ķ���
     * @param validator У��Ĺ���
     * @return an ValidatorRegister after registered specified object -> validator
     * @author Jerry.X.He
     * @date 5/3/2017 10:07 PM
     * @since 1.0
     */
    ValidatorRegister register(Object obj, Validator validator);

    /**
     * ��ȡ���е�ValidateContext
     *
     * @return the iterator that could iterate all ValidateContext
     * @author Jerry.X.He
     * @date 5/3/2017 10:15 PM
     * @since 1.0
     */
    Iterator<ValidateContext> iterator();

    /**
     * ���������Ѿ�ע��� obj -> validator
     *
     * @return the result of all validator validated
     * @author Jerry.X.He
     * @date 5/3/2017 10:07 PM
     * @since 1.0
     */
    Result apply();

}
