package com.hx.log.validator;

import com.hx.log.util.Tools;
import com.hx.log.validator.interf.Validator;
import com.hx.log.validator.interf.ValidatorRegister;
import com.hx.log.validator.validator.*;

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

    // ---------------------------- ValidateRegisters ----------------------------

    /**
     * 创建一个简易的ValidatorRegister
     *
     * @return com.hx.log.validator.interf.ValidatorRegister
     * @author Jerry.X.He
     * @date 5/3/2017 10:28 PM
     * @since 1.0
     */
    public static ValidatorRegister register() {
        return new SimpleValidatorRegister();
    }

    // ---------------------------- Validators ----------------------------

    /**
     * 创建一个校验空字符串的Validator
     *
     * @return com.hx.log.validator.interf.Validator<T>
     * @author Jerry.X.He
     * @date 5/3/2017 11:47 PM
     * @since 1.0
     */
    public static <T> Validator<T> emptyStr() {
        return new StrEmptyValidator<>();
    }

    /**
     * 创建一个校验空对象的Validator
     *
     * @return com.hx.log.validator.interf.Validator<T>
     * @author Jerry.X.He
     * @date 5/3/2017 11:47 PM
     * @since 1.0
     */
    public static <T> Validator<T> emptyObj() {
        return new ObjEmptyValidator<>();
    }

    /**
     * 构造一个校验对象相等的validator
     *
     * @param target 给定的需要比较的对象
     * @return com.hx.log.validator.interf.Validator<T>
     * @author Jerry.X.He
     * @date 5/3/2017 10:01 PM
     * @since 1.0
     */
    public static <T> Validator<T> eq(T target) {
        return EqValidator.eq(target);
    }

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
    public static <T extends Comparable<T>> Validator<T> range(T lowerLimit, T upperLimit,
                                                               boolean containsLowerLimit, boolean containsUpperLimit) {
        return RangeValidator.range(lowerLimit, upperLimit, containsLowerLimit, containsUpperLimit);
    }

    public static <T extends Comparable<T>> Validator<T> range(T lowerLimit, T upperLimit) {
        return RangeValidator.range(lowerLimit, upperLimit);
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
    public static <T extends Comparable<T>> Validator<T> gt(T lowerLimit, boolean containsLowerLimit) {
        return RangeValidator.gt(lowerLimit, containsLowerLimit);
    }

    public static <T extends Comparable<T>> Validator<T> gt(T lowerLimit) {
        return RangeValidator.gt(lowerLimit);
    }

    public static <T extends Comparable<T>> Validator<T> gte(T lowerLimit) {
        return RangeValidator.gte(lowerLimit);
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
    public static <T extends Comparable<T>> Validator<T> lt(T upperLimit, boolean containsUpperLimit) {
        return RangeValidator.lt(upperLimit, containsUpperLimit);
    }

    public static <T extends Comparable<T>> Validator<T> lt(T upperLimit) {
        return RangeValidator.lt(upperLimit);
    }

    public static <T extends Comparable<T>> Validator<T> lte(T upperLimit) {
        return RangeValidator.lt(upperLimit);
    }

    /**
     * 根据给定的regex创建一个RegexValidator
     *
     * @param regex 给定的regex
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static Validator<String> regex(String regex) {
        return new RegexValidator(regex);
    }

    /**
     * 根据给定的prefix创建一个StartsWithValidator
     *
     * @param substr 给定的子串
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static Validator<String> contains(String substr) {
        return new ContainsValidator(substr);
    }

    /**
     * 根据给定的prefix创建一个StartsWithValidator
     *
     * @param startsWith 给定的前缀
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static Validator<String> startsWith(String startsWith) {
        return new StartsWithValidator(startsWith);
    }

    /**
     * 根据给定的suffix创建一个EndsWithValidator
     *
     * @param endsWith 给定的后缀
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static Validator<String> endsWith(String endsWith) {
        return new EndsWithValidator(endsWith);
    }

    /**
     * 根据给定的target创建一个EqIgnoreCaseValidator
     *
     * @param target 给定的目标字符串
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static Validator<String> eqIgnoreCase(String target) {
        return new EqIgnoreCaseValidator(target);
    }

    /**
     * 根据给定的输入, 构造一个AttrHandlerValidator
     *
     * @param handlerStr handlerString
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/4/2017 8:53 PM
     * @since 1.0
     */
    public static Validator<String> handler(String handlerStr) {
        return new AttrHandlerValidator(handlerStr);
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
    public static <T> ValidatorChain<T> chainOf(int capacity) {
        return new ValidatorChain<>(capacity);
    }

    public static <T> ValidatorChain<T> chainOf() {
        return chainOf(10);
    }


}
