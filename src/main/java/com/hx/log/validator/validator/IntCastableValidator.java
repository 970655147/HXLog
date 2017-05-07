package com.hx.log.validator.validator;


import com.hx.log.interf.Result;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResult;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.log.validator.interf.Validator;

/**
 * IntCastableValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 9:35 PM
 */
public class IntCastableValidator implements Validator<Object> {

    /**
     * ����
     */
    private static IntCastableValidator INSTANCE;

    /**
     * ��ʼ��
     *
     * @since 1.0
     */
    private IntCastableValidator() {

    }

    /**
     * ��ȡ��������
     *
     * @return com.hx.log.validator.validator.BooleanCastableValidator
     * @author Jerry.X.He
     * @date 5/6/2017 5:26 PM
     * @since 1.0
     */
    public static IntCastableValidator getInstance() {
        if(INSTANCE == null) {
            synchronized (IntCastableValidator.class) {
                if(INSTANCE == null) {
                    INSTANCE = new IntCastableValidator();
                }
            }
        }

        return INSTANCE;
    }

    @Override
    public Result validate(Object obj, Object extra) {
        if(obj == null) {
            return failed(obj);
        }

        if ((obj instanceof Byte) || (obj instanceof Short)
            || (obj instanceof Integer) || (obj instanceof Long) ) {
            return ValidateResultUtils.success();
        }
        if (obj instanceof String) {
            String str = (String) obj;
            try {
                Integer.valueOf(str);
                return ValidateResultUtils.success();
            } catch (Exception e) {
                // ignore
            }
        }

        return failed(obj);
    }

    /**
     * �Ƚ�ʧ��֮�󷵻صĽ��
     *
     * @return com.hx.log.validator.ValidateResult
     * @author Jerry.X.He
     * @date 5/3/2017 9:40 PM
     * @since 1.0
     */
    private ValidateResult failed(Object obj) {
        return ValidateResultUtils.failed("the obj : " + obj + " can't cast to int !");
    }

}
