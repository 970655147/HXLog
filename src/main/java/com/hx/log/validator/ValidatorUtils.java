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
     * ����һ�����׵�ValidatorRegister
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
     * ����һ��У����ַ�����Validator
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
     * ����һ��У��ն����Validator
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
     * ����һ��У�������ȵ�validator
     *
     * @param target ��������Ҫ�ȽϵĶ���
     * @return com.hx.common.interf.validator.Validator<T>
     * @author Jerry.X.He
     * @date 5/3/2017 10:01 PM
     * @since 1.0
     */
    public static <T> EqValidator<T> eq(T target) {
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
     * ���ݸ�����regex����һ��RegexValidator
     *
     * @param regex ������regex
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static RegexValidator regex(String regex) {
        return new RegexValidator(regex);
    }

    /**
     * ���ݸ�����prefix����һ��StartsWithValidator
     *
     * @param substr �������Ӵ�
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static ContainsValidator contains(String substr) {
        return new ContainsValidator(substr);
    }

    /**
     * ���ݸ�����prefix����һ��StartsWithValidator
     *
     * @param startsWith ������ǰ׺
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static StartsWithValidator startsWith(String startsWith) {
        return new StartsWithValidator(startsWith);
    }

    /**
     * ���ݸ�����suffix����һ��EndsWithValidator
     *
     * @param endsWith �����ĺ�׺
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static EndsWithValidator endsWith(String endsWith) {
        return new EndsWithValidator(endsWith);
    }

    /**
     * ���ݸ�����target����һ��EqIgnoreCaseValidator
     *
     * @param target ������Ŀ���ַ���
     * @return com.hx.common.interf.validator.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static EqIgnoreCaseValidator eqIgnoreCase(String target) {
        return new EqIgnoreCaseValidator(target);
    }

    /**
     * ���ݸ���������, ����һ��AttrHandlerValidator
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
     * ��ȡһ��У������ܹ�ת��Ϊboolean��Validator
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
     * ��ȡһ��У������ܹ�ת��Ϊint��Validator
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
     * ��ȡһ��У������ܹ�ת��Ϊlong��Validator
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
     * ��ȡһ��У������ܹ�ת��ΪFloat��Validator
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
     * ��ȡһ��У������ܹ�ת��ΪDouble��Validator
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
