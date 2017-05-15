package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.common.interf.validator.Validator;

import java.util.regex.Pattern;

/**
 * RegexValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 11:49 PM
 */
public class RegexValidator implements Validator<String> {

    /**
     * 给定的正则
     */
    private String regex;
    /**
     * 根据给定的正则表达式编译的pattern
     */
    private Pattern regexPattern;

    /**
     * 初始化
     *
     * @param regex 给定的正则
     * @author Jerry.X.He <970655147@qq.com>
     * @date 5/3/2017 11:49 PM
     * @since 1.0
     */
    public RegexValidator(String regex) {
        setRegex(regex);
    }

    public RegexValidator() {
    }

    /**
     * setter & getter
     */
    public String getRegex() {
        return regex;
    }

    public RegexValidator setRegex(String regex) {
        Tools.assert0(regex != null, "'regex' can't be null !");
        this.regex = regex;
        this.regexPattern = Pattern.compile(regex);
        return this;
    }

    @Override
    public Result validate(String obj, Object extra) {
        if((obj == null) || (regexPattern == null) ) {
            return failed(obj);
        }

        boolean matches = regexPattern.matcher(obj).find();
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
        return ValidateResultUtils.failed("the String : '" + obj + "' does not match the regex : '" + regex + "' !");
    }

}
