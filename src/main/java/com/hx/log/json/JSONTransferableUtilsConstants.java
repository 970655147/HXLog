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
     * ��ȡidxMap, filterIdxMap��idxManager
     */
    public static final String _JSON_TIDX_MAP_MANAGER = "jsonTIdxMapManager";
    /**
     * id������
     */
    public static final String _JSON_TID = "jsonTId";
    /**
     * ѭ����ʱ�����������
     */
    public static final String _JSON_TFOR_EACH_ELE = "jsonTForEachEle";
    /**
     * BEAN_KEY������
     */
    public static final String _JSON_TBEAN_KEY = "jsonTBeanKey";
    /**
     * PROTO_BEAN������
     */
    public static final String _JSON_TPROTO_BEAN_KEY = "jsonTProtoBeanKey";
    /**
     * arrIdxMap �ƺ������� set, add��, ������
     */
    public static final String _JSON_TARR_IDX_MAP_KEY = "jsonTArrIdxMapKey";
    /**
     * Ĭ�ϵ�loadIdx
     */
    public static final String _JSON_TDEFAULT_LOAD_IDX = "jsonTDefaultLoadIdx";
    /**
     * Ĭ�ϵ�filterIdx
     */
    public static final String _JSON_TDEFAULT_FILTER_IDX = "jsonTDefaultFilterIdx";
    /**
     * ������صĺ�׺
     */
    public static final String _JSON_TIDX_SUFFIX = "jsonTIdxSuffix";
    /**
     * Object��ص���ʱ����ĺ�׺
     */
    public static final String _JSON_TOBJ_SUFFIX = "jsonTObjSuffix";
    /**
     * Array��ص���ʱ����ĺ�׺
     */
    public static final String _JSON_TARR_SUFFIX = "jsonTArrSuffix";

    static {
        /**
         * JSONTransferable ���
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
