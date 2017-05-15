package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.common.interf.validator.Validator;

/**
 * У������������Ƿ��ڸ����ķ�Χ��
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 9:08 PM
 */
public class RangeValidator<T extends Comparable<T>> implements Validator<T> {

    /**
     * ����Ͻ�/ �½�Ϊ��ǰ����, ������ �Ͻ�/ �½�
     */
    private static final Comparable NONE_LIMIT = new Integer(-1);
    /**
     * Ĭ���Ƿ�����½�
     */
    public static final boolean DEFAULT_CONTAINS_LOWER_LIMIT = true;
    /**
     * Ĭ���Ƿ�����Ͻ�
     */
    public static final boolean DEFAULT_CONTAINS_UPPER_LIMIT = true;

    /**
     * У����½�
     */
    private T lowerLimit;
    /**
     * У����Ͻ�
     */
    private T upperLimit;
    /**
     * �Ƿ�����½�
     */
    private boolean containsLowerLimit;
    /**
     * �Ƿ�����Ͻ�
     */
    private boolean containsUpperLimit;

    /**
     * ��ʼ��
     *
     * @param lowerLimit         �½�
     * @param upperLimit         �Ͻ�
     * @param containsLowerLimit �Ƿ�����½�
     * @param containsUpperLimit �Ƿ�����Ͻ�
     * @return
     * @author
     * @date
     * @since 1.0
     */
    public RangeValidator(T lowerLimit, T upperLimit,
                          boolean containsLowerLimit, boolean containsUpperLimit) {
        setLowerLimit(lowerLimit).setUpperLimit(upperLimit);
        setContainsLowerLimit(containsLowerLimit).setContainsUpperLimit(containsUpperLimit);
    }

    public RangeValidator(T lowerLimit, T upperLimit) {
        this(lowerLimit, upperLimit, DEFAULT_CONTAINS_LOWER_LIMIT, DEFAULT_CONTAINS_LOWER_LIMIT);
    }

    public RangeValidator() {
        this((T) NONE_LIMIT, (T) NONE_LIMIT);
    }

    /**
     * ���߷���
     */
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
        return new RangeValidator<T>(lowerLimit, upperLimit, containsLowerLimit, containsUpperLimit);
    }

    public static <T extends Comparable<T>> RangeValidator<T> range(T lowerLimit, T upperLimit) {
        return new RangeValidator(lowerLimit, upperLimit, DEFAULT_CONTAINS_LOWER_LIMIT, DEFAULT_CONTAINS_UPPER_LIMIT);
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
        return new RangeValidator(lowerLimit, NONE_LIMIT, containsLowerLimit, DEFAULT_CONTAINS_UPPER_LIMIT);
    }

    public static <T extends Comparable<T>> RangeValidator<T> gt(T lowerLimit) {
        return new RangeValidator(lowerLimit, NONE_LIMIT, DEFAULT_CONTAINS_LOWER_LIMIT, DEFAULT_CONTAINS_UPPER_LIMIT);
    }

    public static <T extends Comparable<T>> RangeValidator<T> gte(T lowerLimit) {
        return new RangeValidator(lowerLimit, NONE_LIMIT, true, DEFAULT_CONTAINS_UPPER_LIMIT);
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
        return new RangeValidator(NONE_LIMIT, upperLimit, DEFAULT_CONTAINS_LOWER_LIMIT, containsUpperLimit);
    }

    public static <T extends Comparable<T>> RangeValidator<T> lt(T upperLimit) {
        return new RangeValidator(NONE_LIMIT, upperLimit, DEFAULT_CONTAINS_LOWER_LIMIT, DEFAULT_CONTAINS_UPPER_LIMIT);
    }

    public static <T extends Comparable<T>> RangeValidator<T> lte(T upperLimit) {
        return new RangeValidator(NONE_LIMIT, upperLimit, DEFAULT_CONTAINS_LOWER_LIMIT, true);
    }

    /**
     * setter & getter
     */
    public T getLowerLimit() {
        return lowerLimit;
    }

    public RangeValidator<T> setLowerLimit(T lowerLimit) {
        Tools.assert0(lowerLimit != null, "'lowerLimit' can't be null !");
        if(limited(lowerLimit) && limited(upperLimit)) {
            Tools.assert0((lowerLimit.compareTo(upperLimit) <= 0), "'lowerLimit' must lte[<=] upperLimit !");
        }

        this.lowerLimit = lowerLimit;
        return this;
    }

    public T getUpperLimit() {
        return upperLimit;
    }

    public RangeValidator<T> setUpperLimit(T upperLimit) {
        Tools.assert0(upperLimit != null, "'upperLimit' can't be null !");
        if(limited(lowerLimit) && limited(upperLimit)) {
            Tools.assert0((lowerLimit.compareTo(upperLimit) <= 0), "'lowerLimit' must lte[<=] upperLimit !");
        }

        this.upperLimit = upperLimit;
        return this;
    }

    public boolean isContainsLowerLimit() {
        return containsLowerLimit;
    }

    public RangeValidator<T> setContainsLowerLimit(boolean containsLowerLimit) {
        this.containsLowerLimit = containsLowerLimit;
        return this;
    }

    public boolean isContainsUpperLimit() {
        return containsUpperLimit;
    }

    public RangeValidator<T> setContainsUpperLimit(boolean containsUpperLimit) {
        this.containsUpperLimit = containsUpperLimit;
        return this;
    }

    @Override
    public Result validate(T obj, Object extra) {
        if (obj == null) {
            return ValidateResultUtils.failed("'obj' is null !");
        }

        if (limited(lowerLimit)) {
            int comp = obj.compareTo(lowerLimit);
            if (containsLowerLimit ? (comp < 0) : (comp <= 0)) {
                ValidateResultUtils.failed("lt specified bounds !");
            }
        }
        if (limited(upperLimit)) {
            int comp = obj.compareTo(upperLimit);
            if (containsUpperLimit ? (comp > 0) : (comp >= 0)) {
                ValidateResultUtils.failed("gte specified bounds !");
            }
        }

        return ValidateResultUtils.success();
    }

    /**
     * �ж��Ƿ����Ƹ�����limit
     *
     * @param limit ������limit
     * @return boolean
     * @author Jerry.X.He
     * @date 5/3/2017 10:58 PM
     * @since 1.0
     */
    private boolean limited(T limit) {
        return (limit != NONE_LIMIT);
    }

}
