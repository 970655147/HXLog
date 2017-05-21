package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.common.util.ResultUtils;
import com.hx.common.interf.validator.Validator;

import java.util.ArrayList;
import java.util.List;

/**
 * һ�����ϵ�Validator
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
     * ����������û��ͨ��У���validator
     */
    private Validator<T> lastValidator;

    /**
     * ��ʼ��
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
     * ����һ��������������ValidatorChain
     *
     * @param capacity ����������
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
     * setter & getter
     */
    public List<Validator<T>> getChain() {
        return chain;
    }

    /**
     * ���һ��Validator
     *
     * @param validator ������validator
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
            if (!result.isSuccess()) {
                lastValidator = validator;
                return result;
            }
        }

        return ResultUtils.success();
    }

    /**
     * ����������û��ͨ��У���validator
     *
     * @return com.hx.common.interf.validator.Validator
     * @author Jerry.X.He
     * @date 5/4/2017 8:52 PM
     * @since 1.0
     */
    public Validator lastValidator() {
        return lastValidator;
    }

}
