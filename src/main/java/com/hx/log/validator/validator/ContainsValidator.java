package com.hx.log.validator.validator;

import com.hx.log.interf.Result;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.log.validator.interf.Validator;

/**
 * ContainsValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 11:49 PM
 */
public class ContainsValidator implements Validator<String> {

    /**
     * �������Ӵ�
     */
    private String substr;

    /**
     * ��ʼ��
     *
     * @param substr �������ַ���
     * @author Jerry.X.He <970655147@qq.com>
     * @date 5/3/2017 11:49 PM
     * @since 1.0
     */
    public ContainsValidator(String substr) {
        Tools.assert0(substr != null, "'substr' can't be null !");
        this.substr = substr;
    }

    @Override
    public Result validate(String obj, Object extra) {
        if(obj == null) {
            return failed(obj);
        }

        boolean matches = obj.contains(substr);
        if(! matches) {
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
    private Result failed(String obj) {
        return ValidateResultUtils.failed("the String : '" + obj + "' does not contains : '" + substr + "' !");
    }

}
