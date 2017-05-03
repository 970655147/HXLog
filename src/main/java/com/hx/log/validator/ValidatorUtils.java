package com.hx.log.validator;

import com.hx.log.util.Tools;
import com.hx.log.validator.interf.Validator;
import com.hx.log.validator.interf.ValidatorRegister;
import com.hx.log.validator.validator.EqValidator;
import com.hx.log.validator.validator.RangeValidator;
import com.hx.log.validator.validator.ValidatorChain;

/**
 * ValidatorUtils
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 9:55 PM
 */
public final class ValidatorUtils {

    // disable constructor
    private ValidatorUtils() {
        Tools.assert0("can't instantiate !");
    }

    /**
     * ����һ�����׵�ValidatorRegister
     *
     * @return com.hx.log.validator.interf.ValidatorRegister
     * @author Jerry.X.He
     * @date 5/3/2017 10:28 PM
     * @since 1.0
     */
    public static ValidatorRegister register() {
        return new SimpleValidatorRegister();
    }

    /**
     * ����һ��У�������ȵ�validator
     *
     * @param target ��������Ҫ�ȽϵĶ���
     * @return com.hx.log.validator.interf.Validator<T>
     * @author Jerry.X.He
     * @date 5/3/2017 10:01 PM
     * @since 1.0
     */
    public static <T> Validator<T> eq(T target) {
        return EqValidator.eq(target);
    }

    /**
     * ����һ���������½��Validator
     *
     * @param lowerLimit         �½�
     * @param upperLimit         �Ͻ�
     * @param containsLowerLimit �Ƿ�����½�
     * @param containsUpperLimit �Ƿ�����Ͻ�
     * @return com.hx.log.validator.validator.RangeValidator
     * @author Jerry.X.He
     * @date 5/3/2017 9:20 PM
     * @since 1.0
     */
    public static <T extends Comparable<T>> RangeValidator<T> range(T lowerLimit, T upperLimit,
                                                                    boolean containsLowerLimit, boolean containsUpperLimit) {
        return RangeValidator.range(lowerLimit, upperLimit, containsLowerLimit, containsUpperLimit);
    }

    public static <T extends Comparable<T>> RangeValidator<T> range(T lowerLimit, T upperLimit) {
        return RangeValidator.range(lowerLimit, upperLimit);
    }

    /**
     * �������Ƹ�����object���½�
     *
     * @param lowerLimit         �½�
     * @param containsLowerLimit �Ƿ�����½�
     * @return com.hx.log.validator.validator.RangeValidator
     * @author Jerry.X.He
     * @date 5/3/2017 9:15 PM
     * @since 1.0
     */
    public static <T extends Comparable<T>> RangeValidator<T> gt(T lowerLimit, boolean containsLowerLimit) {
        return RangeValidator.gt(lowerLimit, containsLowerLimit);
    }

    public static <T extends Comparable<T>> RangeValidator<T> gt(T lowerLimit) {
        return RangeValidator.gt(lowerLimit);
    }

    public static <T extends Comparable<T>> RangeValidator<T> gte(T lowerLimit) {
        return RangeValidator.gte(lowerLimit);
    }

    /**
     * �������Ƹ�����object���Ͻ�
     *
     * @param upperLimit         �Ͻ�
     * @param containsUpperLimit �Ƿ�����Ͻ�
     * @return com.hx.log.validator.validator.RangeValidator
     * @author Jerry.X.He
     * @date 5/3/2017 9:15 PM
     * @since 1.0
     */
    public static <T extends Comparable<T>> RangeValidator<T> lt(T upperLimit, boolean containsUpperLimit) {
        return RangeValidator.lt(upperLimit, containsUpperLimit);
    }

    public static <T extends Comparable<T>> RangeValidator<T> lt(T upperLimit) {
        return RangeValidator.lt(upperLimit);
    }

    public static <T extends Comparable<T>> RangeValidator<T> lte(T upperLimit) {
        return RangeValidator.lt(upperLimit);
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
    public static <T> ValidatorChain<T> chainOf(int capacity) {
        return new ValidatorChain<>(capacity);
    }

    public static <T> ValidatorChain<T> chainOf() {
        return chainOf(10);
    }



}
