package com.hx.log.validator.validator;

import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.common.util.ResultUtils;
import com.hx.common.interf.validator.Validator;

import java.util.Collection;
import java.util.Map;

/**
 * ObjEmptyValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 11:41 PM
 */
public class ObjEmptyValidator implements Validator<Object> {

    /**
     * 单例
     */
    private static ObjEmptyValidator INSTANCE;

    /**
     * 初始化
     *
     * @since 1.0
     */
    private ObjEmptyValidator() {

    }

    /**
     * 获取单例对象
     *
     * @return com.hx.log.validator.validator.BooleanCastableValidator
     * @author Jerry.X.He
     * @date 5/6/2017 5:26 PM
     * @since 1.0
     */
    public static ObjEmptyValidator getInstance() {
        if(INSTANCE == null) {
            synchronized (ObjEmptyValidator.class) {
                if(INSTANCE == null) {
                    INSTANCE = new ObjEmptyValidator();
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
        if(Tools.isEmpty(String.valueOf(obj)) ) {
            return failed(obj);
        }
        if((obj instanceof Collection) && (Tools.isEmpty((Collection) obj)) ) {
            return failed(obj);
        }
        if((obj instanceof Map) && (Tools.isEmpty((Map) obj)) ) {
            return failed(obj);
        }
        if((obj.getClass().isArray()) && Tools.isEmpty((Object[]) obj) ) {
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
    public Result failed(Object obj) {
        return ResultUtils.failed("the Object : '" + String.valueOf(obj) + "' is null or empty !");
    }

}
