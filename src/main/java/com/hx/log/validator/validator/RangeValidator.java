package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.common.interf.validator.Validator;

/**
 * 校验给定的输入是否在给定的范围内
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 9:08 PM
 */
public class RangeValidator<T extends Comparable<T>> implements Validator<T> {

    /**
     * 如果上界/ 下界为当前对象, 不限制 上界/ 下界
     */
    private static final Comparable NONE_LIMIT = new Integer(-1);
    /**
     * 默认是否包含下界
     */
    public static final boolean DEFAULT_CONTAINS_LOWER_LIMIT = true;
    /**
     * 默认是否包含上界
     */
    public static final boolean DEFAULT_CONTAINS_UPPER_LIMIT = true;

    /**
     * 校验的下界
     */
    private T lowerLimit;
    /**
     * 校验的上界
     */
    private T upperLimit;
    /**
     * 是否包含下界
     */
    private boolean containsLowerLimit;
    /**
     * 是否包含上界
     */
    private boolean containsUpperLimit;

    /**
     * 初始化
     *
     * @param lowerLimit         下界
     * @param upperLimit         上界
     * @param containsLowerLimit 是否包含下界
     * @param containsUpperLimit 是否包含上界
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
     * 工具方法
     */
    /**
     * 创建一个限制上下界的Validator
     *
     * @param lowerLimit         下界
     * @param upperLimit         上界
     * @param containsLowerLimit 是否包含下界
     * @param containsUpperLimit 是否包含上界
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
     * 仅仅限制给定的object的下界
     *
     * @param lowerLimit         下界
     * @param containsLowerLimit 是否包含下界
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
     * 仅仅限制给定的object的上界
     *
     * @param upperLimit         上界
     * @param containsUpperLimit 是否包含上界
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
     * 判断是否限制给定的limit
     *
     * @param limit 给定的limit
     * @return boolean
     * @author Jerry.X.He
     * @date 5/3/2017 10:58 PM
     * @since 1.0
     */
    private boolean limited(T limit) {
        return (limit != NONE_LIMIT);
    }

}
