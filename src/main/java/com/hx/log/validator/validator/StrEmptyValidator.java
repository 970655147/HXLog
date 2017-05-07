package com.hx.log.validator.validator;

import com.hx.log.interf.Result;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.log.validator.interf.Validator;

/**
 * StrEmptyValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 11:41 PM
 */
public class StrEmptyValidator implements Validator<Object> {

    /**
     * ����
     */
    private static StrEmptyValidator INSTANCE;

    /**
     * ��ʼ��
     *
     * @since 1.0
     */
    private StrEmptyValidator() {

    }

    /**
     * ��ȡ��������
     *
     * @return com.hx.log.validator.validator.BooleanCastableValidator
     * @author Jerry.X.He
     * @date 5/6/2017 5:26 PM
     * @since 1.0
     */
    public static StrEmptyValidator getInstance() {
        if (INSTANCE == null) {
            synchronized (StrEmptyValidator.class) {
                if (INSTANCE == null) {
                    INSTANCE = new StrEmptyValidator();
                }
            }
        }

        return INSTANCE;
    }

    @Override
    public Result validate(Object obj, Object extra) {
        if (obj == null) {
            return failed(obj);
        }
        if (Tools.isEmpty(String.valueOf(obj))) {
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
    public Result failed(Object obj) {
        return ValidateResultUtils.failed("the Object : '" + String.valueOf(obj) + "' is null or empty !");
    }

}
