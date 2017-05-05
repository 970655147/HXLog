package com.hx.log.json.interf;

import com.hx.json.JSONObject;

/**
 * 校验给定的JSONObject的必需属性的函数
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 2017-03-19 18:44
 */
public interface CheckRequiredAttributeFunc {

    /**
     * 校验给定的JSONObject的给定属性
     *
     * @param obj  需要校验的JSONObject
     * @param attr 需要校验的属性
     * @return return true if 'attr' of 'obj' is validate, or else
     * @author 970655147 created at 2017-03-19 18:41
     */
    boolean check(JSONObject obj, String attr);

}
