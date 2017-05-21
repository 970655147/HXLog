package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.common.util.ResultUtils;
import com.hx.log.util.Tools;
import com.hx.common.interf.validator.Validator;

/**
 * StartsWithValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 11:49 PM
 */
public class StartsWithValidator implements Validator<String> {

    /**
     * �������ַ���
     */
    private String startsWith;

    /**
     * ��ʼ��
     *
     * @param startsWith �������ַ���
     * @author Jerry.X.He <970655147@qq.com>
     * @date 5/3/2017 11:49 PM
     * @since 1.0
     */
    public StartsWithValidator(String startsWith) {
        setStartsWith(startsWith);
    }

    public StartsWithValidator() {
    }

    /**
     * setter & getter
     */
    public String getStartsWith() {
        return startsWith;
    }

    public StartsWithValidator setStartsWith(String startsWith) {
        Tools.assert0(startsWith != null, "'startsWith' can't be null !");
        this.startsWith = startsWith;
        return this;
    }

    @Override
    public Result validate(String obj, Object extra) {
        if((obj == null) || (startsWith == null) ) {
            return failed(obj);
        }

        boolean matches = obj.startsWith(startsWith);
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
        return ResultUtils.failed("the String : '" + obj + "' does not startsWith : '" + startsWith + "' !");
    }

}
