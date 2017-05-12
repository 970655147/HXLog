package com.hx.log.alogrithm.tree.interf;

import com.hx.json.JSONObject;

import java.io.File;

/**
 * ���������ļ�����Ϣ ��װ�������Ľ����Ϣ��
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/12/2017 9:36 PM
 */
public interface EncapFileInfo {

    /**
     * �ռ�������file����Ϣ��result��
     *
     * @param file   �������ļ�
     * @param result �ռ������JSONObject
     * @return void
     * @author Jerry.X.He
     * @date 5/12/2017 9:36 PM
     * @since 1.0
     */
    JSONObject encapFileInfo(File file, JSONObject result);

}
