package com.hx.log.validator;

import com.hx.log.util.Tools;
import com.hx.common.interf.validator.ValidatorRegister;
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
     * @return com.hx.common.interf.validator.ValidatorRegister
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
     * @return com.hx.common.interf.validator.Validator<T>
     * @author Jerry.X.He
     * @date 5/3/2017 11:47 PM
     * @since 1.0
     */
    public static <T> StrEmptyValidator emptyStr() {
        return StrEmptyValidator.getInstance();
    }

    /**
     * 创建一个校验空对象的Validator
     *
     * @return com.hx.common.interf.validator.Validator<T>
     * @author Jerry.X.He
     * @date 5/3/2017 11:47 PM
     * @since 1.0
     */
    public static <T> ObjEmptyValidator emptyObj() {
        return ObjEmptyValidator.getInstance();
    }

    /**
     * 构造一个校验对象相等的validator
     *
     * @param target 给定的需要比较的对象
     * @return com.hx.common.interf.validator.Validator<T>
     * @author Jerry.X.He
     * @date 5/3/2017 10:01 PM
     * @since 1.0
     */
    public static <T> EqValidator<T> eq(T target) {
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
    public static <T extends Comparable<T>> RangeValidator<T> range(T lowerLimit, T upperLimit,
                                                                    boolean containsLowerLimit, boolean containsUpperLimit) {
        return RangeValidator.range(lowerLimit, upperLimit, containsLowerLimit, containsUpperLimit);
    }

    public static <T extends Comparable<T>> RangeValidator<T> range(T lowerLimit, T upperLimit) {
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
        return RangeValidator.lt(upperLimit, containsUpperLimit);
    }

    public static <T extends Comparable<T>> RangeValidator<T> lt(T upperLimit) {
        return RangeValidator.lt(upperLimit);
    }

    public static <T extends Comparable<T>> RangeValidator<T> lte(T upperLimit) {
        return RangeValidator.lt(upperLimit);
    }

    /**
     * 根据给定的regex创建一个RegexValidator
     *
     * @param regex 给定的regex
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static RegexValidator regex(String regex) {
        return new RegexValidator(regex);
    }

    /**
     * 根据给定的prefix创建一个StartsWithValidator
     *
     * @param substr 给定的子串
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static ContainsValidator contains(String substr) {
        return new ContainsValidator(substr);
    }

    /**
     * 根据给定的prefix创建一个StartsWithValidator
     *
     * @param startsWith 给定的前缀
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static StartsWithValidator startsWith(String startsWith) {
        return new StartsWithValidator(startsWith);
    }

    /**
     * 根据给定的suffix创建一个EndsWithValidator
     *
     * @param endsWith 给定的后缀
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static EndsWithValidator endsWith(String endsWith) {
        return new EndsWithValidator(endsWith);
    }

    /**
     * 根据给定的target创建一个EqIgnoreCaseValidator
     *
     * @param target 给定的目标字符串
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static EqIgnoreCaseValidator eqIgnoreCase(String target) {
        return new EqIgnoreCaseValidator(target);
    }

    /**
     * 根据给定的输入, 构造一个AttrHandlerValidator
     *
     * @param handlerStr handlerString
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/4/2017 8:53 PM
     * @since 1.0
     */
    public static AttrHandlerValidator handler(String handlerStr) {
        return new AttrHandlerValidator(handlerStr);
    }

    /**
     * 获取一个校验对象能够转换为boolean的Validator
     *
     * @return com.hx.log.validator.validator.BooleanCastableValidator
     * @author Jerry.X.He
     * @date 5/6/2017 5:30 PM
     * @since 1.0
     */
    public static BooleanCastableValidator booleanable() {
        return BooleanCastableValidator.getInstance();
    }

    /**
     * 获取一个校验对象能够转换为int的Validator
     *
     * @return com.hx.log.validator.validator.BooleanCastableValidator
     * @author Jerry.X.He
     * @date 5/6/2017 5:30 PM
     * @since 1.0
     */
    public static IntCastableValidator intable() {
        return IntCastableValidator.getInstance();
    }

    /**
     * 获取一个校验对象能够转换为long的Validator
     *
     * @return com.hx.log.validator.validator.BooleanCastableValidator
     * @author Jerry.X.He
     * @date 5/6/2017 5:30 PM
     * @since 1.0
     */
    public static LongCastableValidator longable() {
        return LongCastableValidator.getInstance();
    }

    /**
     * 获取一个校验对象能够转换为Float的Validator
     *
     * @return com.hx.log.validator.validator.BooleanCastableValidator
     * @author Jerry.X.He
     * @date 5/6/2017 5:30 PM
     * @since 1.0
     */
    public static FloatCastableValidator floatable() {
        return FloatCastableValidator.getInstance();
    }

    /**
     * 获取一个校验对象能够转换为Double的Validator
     *
     * @return com.hx.log.validator.validator.BooleanCastableValidator
     * @author Jerry.X.He
     * @date 5/6/2017 5:30 PM
     * @since 1.0
     */
    public static DoubleCastableValidator doubleable() {
        return DoubleCastableValidator.getInstance();
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
