package com.hx.log.json.interf;

import com.hx.json.JSONObject;

/**
 * У�������JSONObject�ı������Եĺ���
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 2017-03-19 18:44
 */
public interface CheckRequiredAttributeFunc {

    /**
     * У�������JSONObject�ĸ�������
     *
     * @param obj  ��ҪУ���JSONObject
     * @param attr ��ҪУ�������
     * @return return true if 'attr' of 'obj' is validate, or else
     * @author 970655147 created at 2017-03-19 18:41
     */
    boolean check(JSONObject obj, String attr);

}
