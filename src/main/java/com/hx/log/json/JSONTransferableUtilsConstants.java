package com.hx.log.json;

import com.hx.log.util.Constants;
import com.hx.log.util.InnerTools;

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
    public static final String _JSON_TUTILS = "jsonTUtils";
    public static final String _JSON_TIDX_MAP_MANAGER = "jsonTIdxMapManager";
    public static final String _JSON_TID = "jsonTId";
    public static final String _JSON_TFOR_EACH_ELE = "jsonTForEachEle";
    public static final String _JSON_TBEAN_KEY = "jsonTBeanKey";
    public static final String _JSON_TPROTO_BEAN_KEY = "jsonTProtoBeanKey";
    public static final String _JSON_TARR_IDX_MAP_KEY = "jsonTArrIdxMapKey";
    public static final String _JSON_TDEFAULT_LOAD_IDX = "jsonTDefaultLoadIdx";
    public static final String _JSON_TDEFAULT_FILTER_IDX = "jsonTDefaultFilterIdx";
    public static final String _JSON_TIDX_SUFFIX = "jsonTIdxSuffix";
    public static final String _JSON_TOBJ_SUFFIX = "jsonTObjSuffix";
    public static final String _JSON_TARR_SUFFIX = "jsonTArrSuffix";

    static {
        /**
         * JSONTransferable 相关
         */
        Constants.DEFAULT_PROPS.put(_JSON_TUTILS, "Tools");
        Constants.DEFAULT_PROPS.put(_JSON_TIDX_MAP_MANAGER, "Constants");
        Constants.DEFAULT_PROPS.put(_JSON_TID, "ID");
        Constants.DEFAULT_PROPS.put(_JSON_TFOR_EACH_ELE, "ele");
        Constants.DEFAULT_PROPS.put(_JSON_TBEAN_KEY, "BEAN_KEY");
        Constants.DEFAULT_PROPS.put(_JSON_TPROTO_BEAN_KEY, "PROTO_BEAN");
        Constants.DEFAULT_PROPS.put(_JSON_TARR_IDX_MAP_KEY, "arrIdxMap");
        Constants.DEFAULT_PROPS.put(_JSON_TDEFAULT_LOAD_IDX, "CAMEL");
        Constants.DEFAULT_PROPS.put(_JSON_TDEFAULT_FILTER_IDX, "ALL");
        Constants.DEFAULT_PROPS.put(_JSON_TIDX_SUFFIX, "Idxes");
        Constants.DEFAULT_PROPS.put(_JSON_TOBJ_SUFFIX, "Obj");
        Constants.DEFAULT_PROPS.put(_JSON_TARR_SUFFIX, "Arr");
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
