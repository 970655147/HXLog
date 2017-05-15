package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.common.interf.validator.Validator;

/**
 * EqIgnoreCaseValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 11:49 PM
 */
public class EqIgnoreCaseValidator implements Validator<String> {

    /**
     * 给定的字符串
     */
    private String target;

    /**
     * 初始化
     *
     * @param target 给定的字符串
     * @author Jerry.X.He <970655147@qq.com>
     * @date 5/3/2017 11:49 PM
     * @since 1.0
     */
    public EqIgnoreCaseValidator(String target) {
        setTarget(target);
    }

    public EqIgnoreCaseValidator() {
    }

    /**
     * setter & getter
     */
    public String getTarget() {
        return target;
    }

    public EqIgnoreCaseValidator setTarget(String target) {
        this.target = target;
        return this;
    }

    @Override
    public Result validate(String obj, Object extra) {
        if(obj == null) {
            if(target != null) {
                return failed(obj);
            }
            return ValidateResultUtils.success();
        }

        boolean matches = Tools.equalsIgnoreCase(obj, target);
        if(! matches) {
            return failed(obj);
        }

        return ValidateResultUtils.success();
    }

    /**
     * 比较失败之后返回的结果
     *
     * @return com.hx.log.validator.ValidateResult
     * @author Jerry.X.He
     * @date 5/3/2017 9:40 PM
     * @since 1.0
     */
    private Result failed(String obj) {
        return ValidateResultUtils.failed("the String : '" + obj + "' does not equalsIgnoreCase with : '" + target + "' !");
    }

}
