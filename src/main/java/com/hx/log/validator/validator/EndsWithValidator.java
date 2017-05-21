package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.common.util.ResultUtils;
import com.hx.common.interf.validator.Validator;

/**
 * StartsWithValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 11:49 PM
 */
public class EndsWithValidator implements Validator<String> {

    /**
     * 给定的字符串
     */
    private String endsWith;

    /**
     * 初始化
     *
     * @param endsWith 给定的字符串
     * @author Jerry.X.He <970655147@qq.com>
     * @date 5/3/2017 11:49 PM
     * @since 1.0
     */
    public EndsWithValidator(String endsWith) {
        setEndsWith(endsWith);
    }

    public EndsWithValidator() {
    }

    /**
     * setter & getter
     */
    public String getEndsWith() {
        return endsWith;
    }

    public EndsWithValidator setEndsWith(String endsWith) {
        Tools.assert0(endsWith != null, "'endsWith' can't be null !");
        this.endsWith = endsWith;
        return this;
    }

    @Override
    public Result validate(String obj, Object extra) {
        if((obj == null) || (endsWith == null) ) {
            return failed(obj);
        }

        boolean matches = obj.endsWith(endsWith);
        if(! matches) {
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
    private Result failed(String obj) {
        return ResultUtils.failed("the String : '" + obj + "' does not endsWith : '" + endsWith + "' !");
    }

}
