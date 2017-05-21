package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.common.util.ResultUtils;
import com.hx.log.util.Tools;
import com.hx.common.interf.validator.Validator;

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
        setSubstr(substr);
    }

    public ContainsValidator() {
    }

    /**
     * setter & getter
     */
    public String getSubstr() {
        return substr;
    }

    public ContainsValidator setSubstr(String substr) {
        Tools.assert0(substr != null, "'substr' can't be null !");
        this.substr = substr;
        return this;
    }

    @Override
    public Result validate(String obj, Object extra) {
        if((obj == null) || (substr == null) ) {
            return failed(obj);
        }

        boolean matches = obj.contains(substr);
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
        return ResultUtils.failed("the String : '" + obj + "' does not contains : '" + substr + "' !");
    }

}
