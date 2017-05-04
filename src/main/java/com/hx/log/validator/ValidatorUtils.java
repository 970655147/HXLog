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

    // ---------------------------- Validators ----------------------------

    /**
     * ����һ��У����ַ�����Validator
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
     * ����һ��У��ն����Validator
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
    public static <T extends Comparable<T>> Validator<T> range(T lowerLimit, T upperLimit,
                                                               boolean containsLowerLimit, boolean containsUpperLimit) {
        return RangeValidator.range(lowerLimit, upperLimit, containsLowerLimit, containsUpperLimit);
    }

    public static <T extends Comparable<T>> Validator<T> range(T lowerLimit, T upperLimit) {
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
     * �������Ƹ�����object���Ͻ�
     *
     * @param upperLimit         �Ͻ�
     * @param containsUpperLimit �Ƿ�����Ͻ�
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
     * ���ݸ�����regex����һ��RegexValidator
     *
     * @param regex ������regex
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static Validator<String> regex(String regex) {
        return new RegexValidator(regex);
    }

    /**
     * ���ݸ�����prefix����һ��StartsWithValidator
     *
     * @param substr �������Ӵ�
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static Validator<String> contains(String substr) {
        return new ContainsValidator(substr);
    }

    /**
     * ���ݸ�����prefix����һ��StartsWithValidator
     *
     * @param startsWith ������ǰ׺
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static Validator<String> startsWith(String startsWith) {
        return new StartsWithValidator(startsWith);
    }

    /**
     * ���ݸ�����suffix����һ��EndsWithValidator
     *
     * @param endsWith �����ĺ�׺
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static Validator<String> endsWith(String endsWith) {
        return new EndsWithValidator(endsWith);
    }

    /**
     * ���ݸ�����target����һ��EqIgnoreCaseValidator
     *
     * @param target ������Ŀ���ַ���
     * @return com.hx.log.validator.interf.Validator<java.lang.String>
     * @author Jerry.X.He
     * @date 5/3/2017 11:54 PM
     * @since 1.0
     */
    public static Validator<String> eqIgnoreCase(String target) {
        return new EqIgnoreCaseValidator(target);
    }

    /**
     * ���ݸ���������, ����һ��AttrHandlerValidator
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
