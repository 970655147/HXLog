package com.hx.log.validator.validator;


import com.hx.log.interf.Result;
import com.hx.log.validator.ValidateResult;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.log.validator.interf.Validator;

/**
 * EqValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 9:35 PM
 */
public class EqValidator<T> implements Validator<T> {

    /**
     * ��Ҫ�ȽϵĶ���
     */
    private T target;

    /**
     * ��ʼ��
     *
     * @param target ��Ҫ�ȽϵĶ���
     * @since 1.0
     */
    public EqValidator(T target) {
        setTarget(target);
    }

    public EqValidator() {
    }

    /**
     * ����һ��ȷ��obj, target��ȵ�Validatable
     *
     * @param target ��Ҫ�Ƚϵ�Ŀ�����
     * @return com.hx.log.validator.validator.EqValidator
     * @author Jerry.X.He
     * @date 5/3/2017 9:37 PM
     * @since 1.0
     */
    public static <T> EqValidator<T> eq(T target) {
        return new EqValidator<T>(target);
    }

    /**
     * setter & getter
     */
    public T getTarget() {
        return target;
    }

    public EqValidator<T> setTarget(T target) {
        this.target = target;
        return this;
    }

    @Override
    public Result validate(T obj, Object extra) {
        if (obj == null) {
            if (target != null) {
                return failed(obj);
            }
            return ValidateResultUtils.success();
        }

        boolean eq = obj.equals(target);
        if(! eq) {
            return failed(obj);
        }

        return ValidateResultUtils.success();
    }

    /**
     * �Ƚ�ʧ��֮�󷵻صĽ��
     *
     * @return com.hx.log.validator.ValidateResult
     * @author Jerry.X.He
     * @date 5/3/2017 9:40 PM
     * @since 1.0
     */
    private ValidateResult failed(Object obj) {
        return ValidateResultUtils.failed("not eq between : '" + String.valueOf(obj) + "', '" + String.valueOf(target) + "'");
    }

}
