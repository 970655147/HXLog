package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.common.util.ResultUtils;
import com.hx.log.util.Tools;
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
     * �������ַ���
     */
    private String target;

    /**
     * ��ʼ��
     *
     * @param target �������ַ���
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
            return ResultUtils.success();
        }

        boolean matches = Tools.equalsIgnoreCase(obj, target);
        if(! matches) {
            return failed(obj);
        }

        return ResultUtils.success();
    }

    /**
     * �Ƚ�ʧ��֮�󷵻صĽ��
     *
     * @return com.hx.common.result.SimpleResult
     * @author Jerry.X.He
     * @date 5/3/2017 9:40 PM
     * @since 1.0
     */
    private Result failed(String obj) {
        return ResultUtils.failed("the String : '" + obj + "' does not equalsIgnoreCase with : '" + target + "' !");
    }

}
