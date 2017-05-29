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
     * JSONTransferableUtils的key相关的常量
     */
    /**
     * 工具类的类名
     */
    public static final String _JSON_TUTILS = "jsonTUtils";
    /**
     * id的名称
     */
    public static final String _JSON_TID = "jsonTId";
    /**
     * BEAN_KEY的名称
     */
    public static final String _JSON_TBEAN_KEY = "jsonTBeanKey";
    /**
     * PROTO_BEAN的名称
     */
    public static final String _JSON_TPROTO_BEAN_KEY = "jsonTProtoBeanKey";

    static {
        /**
         * JSONTransferable 相关
         */
        Constants.DEFAULT_PROPS.put(_JSON_TUTILS, "Tools");
        Constants.DEFAULT_PROPS.put(_JSON_TID, "ID");
        Constants.DEFAULT_PROPS.put(_JSON_TPROTO_BEAN_KEY, "PROTO_BEAN");
    }

    /**
     * 加载Constants中的默认常量
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
