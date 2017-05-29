package com.hx.log.json;

import com.hx.log.util.Constants;
import com.hx.common.util.InnerTools;

/**
 * JSONTransferableUtilsConstants
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/1/2017 12:03 PM
 */
public final class JSONTransferableUtilsConstants {

    // disable constructor
    private JSONTransferableUtilsConstants() {
        InnerTools.assert0(false, "can't instantiate !");
    }

    /**
     * JSONTransferableUtils��key��صĳ���
     */
    /**
     * �����������
     */
    public static final String _JSON_TUTILS = "jsonTUtils";
    /**
     * id������
     */
    public static final String _JSON_TID = "jsonTId";
    /**
     * BEAN_KEY������
     */
    public static final String _JSON_TBEAN_KEY = "jsonTBeanKey";
    /**
     * PROTO_BEAN������
     */
    public static final String _JSON_TPROTO_BEAN_KEY = "jsonTProtoBeanKey";

    static {
        /**
         * JSONTransferable ���
         */
        Constants.DEFAULT_PROPS.put(_JSON_TUTILS, "Tools");
        Constants.DEFAULT_PROPS.put(_JSON_TID, "ID");
        Constants.DEFAULT_PROPS.put(_JSON_TPROTO_BEAN_KEY, "PROTO_BEAN");
    }

    /**
     * ����Constants�е�Ĭ�ϳ���
     *
     * @return void
     * @author Jerry.X.He
     * @date 5/1/2017 12:14 PM
     * @since 1.0
     */
    public static void loadDefaults() {
        // invoke classloader load current class
    }

}
