package com.hx.log.validator.validator;

import com.hx.log.interf.Result;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResult;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.log.validator.interf.Validator;

import java.util.ArrayList;
import java.util.List;

/**
 * 一个复合的Validator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 9:45 PM
 */
public class ValidatorChain<T> implements Validator<T> {

    /**
     * validatorChain
     */
    private List<Validator<T>> chain;
    /**
     * 给定的输入没有通过校验的validator
     */
    private Validator<T> lastValidator;

    /**
     * 初始化
     *
     * @return
     * @author
     * @date
     * @since 1.0
     */
    public ValidatorChain(int capacity) {
        chain = new ArrayList<>(capacity);
    }

    public ValidatorChain() {
        this(10);
    }

    /**
     * 创建一个给定的容量的ValidatorChain
     *
     * @param capacity 给定的容量
     * @return com.hx.log.validator.validator.ValidatorChain<T>
     * @author Jerry.X.He
     * @date 5/3/2017 10:00 PM
     * @since 1.0
     */
    public static <T> ValidatorChain<T> of(int capacity) {
        return new ValidatorChain<>(capacity);
    }

    public static <T> ValidatorChain<T> of() {
        return of(10);
    }

    /**
     * 添加一个Validator
     *
     * @param validator 给定的validator
     * @return void
     * @author Jerry.X.He
     * @date 5/3/2017 9:51 PM
     * @since 1.0
     */
    public ValidatorChain<T> addValidatable(Validator<T> validator) {
        Tools.assert0(validator != null, "'validator' can't be null !");
        chain.add(validator);
        return this;
    }

    @Override
    public Result validate(T obj, Object extra) {
        for (Validator<T> validator : chain) {
            Result result = validator.validate(obj, extra);
            if (!result.success()) {
                lastValidator = validator;
                return result;
            }
        }

        return ValidateResultUtils.success();
    }

    /**
     * 给定的输入没有通过校验的validator
     *
     * @return com.hx.log.validator.interf.Validator
     * @author Jerry.X.He
     * @date 5/4/2017 8:52 PM
     * @since 1.0
     */
    public Validator lastValidator() {
        return lastValidator;
    }

}
