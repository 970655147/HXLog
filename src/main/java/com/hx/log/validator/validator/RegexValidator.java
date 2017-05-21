package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.common.util.ResultUtils;
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
     * ����������
     */
    private String regex;
    /**
     * ���ݸ�����������ʽ�����pattern
     */
    private Pattern regexPattern;

    /**
     * ��ʼ��
     *
     * @param regex ����������
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
        return ResultUtils.failed("the String : '" + obj + "' does not match the regex : '" + regex + "' !");
    }

}
