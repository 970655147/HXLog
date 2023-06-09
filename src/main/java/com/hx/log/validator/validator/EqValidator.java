package com.hx.log.validator.validator;


import com.hx.common.interf.common.Result;
import com.hx.common.util.ResultUtils;
import com.hx.common.interf.validator.Validator;

/**
 * EqValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 9:35 PM
 */
public class EqValidator<T> implements Validator<T> {

    /**
     * 需要比较的对象
     */
    private T target;

    /**
     * 初始化
     *
     * @param target 需要比较的对象
     * @since 1.0
     */
    public EqValidator(T target) {
        setTarget(target);
    }

    public EqValidator() {
    }

    /**
     * 创建一个确保obj, target相等的Validatable
     *
     * @param target 需要比较的目标对象
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
            return ResultUtils.success();
        }

        boolean eq = obj.equals(target);
        if(! eq) {
            return failed(obj);
        }

        return ResultUtils.success();
    }

    /**
     * 比较失败之后返回的结果
     *
     * @return com.hx.common.result.SimpleResult
     * @author Jerry.X.He
     * @date 5/3/2017 9:40 PM
     * @since 1.0
     */
    private Result failed(Object obj) {
        return ResultUtils.failed("not eq between : '" + String.valueOf(obj) + "', '" + String.valueOf(target) + "'");
    }

}
