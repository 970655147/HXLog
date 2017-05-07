package com.hx.log.validator.validator;

import com.hx.log.interf.Result;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.log.validator.interf.Validator;

/**
 * StartsWithValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 11:49 PM
 */
public class StartsWithValidator implements Validator<String> {

    /**
     * 给定的字符串
     */
    private String startsWith;

    /**
     * 初始化
     *
     * @param startsWith 给定的字符串
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
        return ValidateResultUtils.failed("the String : '" + obj + "' does not startsWith : '" + startsWith + "' !");
    }

}
